#include "relaxation.h"
#include "passes.h"
#include "cleanup.h"
#include "loop.h"

#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/None.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Transforms/Utils/Cloning.h"

#include <unordered_map>
#include <set>
#include <tuple>
#include <string>
#include <sstream>
#include <algorithm>
#include <filesystem>
#include <fstream>

#include "yaml-cpp/yaml.h"

// TODO: Everything printed to errs() should be printed to outs()
// but only if we're in debug mode, and everything originally printed
// to outs() should be directly written to a YAML file
#define DEBUG_TYPE "flagging"

using namespace llvm;
using namespace std;
namespace fs = std::filesystem;

extern cl::opt<string> InputFilename;
extern cl::opt<string> OutputFolder;

// TODO: make this a utility struct
// (advantage is we can define a stream operator to serialize it elegantly)
struct FunctionInfo {
    ValueSet transmitted_vals;
    unordered_set<int> leaked_arg_idxs;
    bool pure_transmitter; // TODO: Rename this to "pseudo transmitter" to match the paper's terminology
    bool degenerate_case; // TODO: Rename this to "trivial case" to match the paper's terminology
};

void add_specbarr_to_block(BasicBlock& BB) {
    LLVMContext& ctx = BB.getParent()->getContext();
    IRBuilder<> IRB(ctx);
    IRB.SetInsertPoint(&BB, BB.getFirstInsertionPt());
    auto func_type = FunctionType::get(Type::getVoidTy(ctx), std::nullopt, false);
    auto barrier_inst = InlineAsm::get(func_type, "lfence", "", true);
    IRB.CreateCall(barrier_inst, std::nullopt);
}

// TODO: make this a utility function
vector<Function*> get_called_funcs(const CallGraph& CG,
                                   const Function* Caller);


bool print_origin(const Value* V) {
    auto I = dyn_cast<Instruction>(V);
    if (!I) return false;
    errs() << to_string(I->getParent()) << "::" << to_string(I);
    return true;
}


void vals_leaked_by_func_calls(const Function* func,
                               const FunctionInfo& func_info,
                               const BlockSet& func_call_sites,
                               EdgeLevelAnalysis& leakage_info,
                               ValueToValueMapTy& VMapToExpanded,
                               ValueSet& leaked_values)
{
    for (auto BB : func_call_sites) {
        auto BB_mapped = dyn_cast<BasicBlock>(VMapToExpanded[BB]);
        for (auto& I : *BB_mapped) {
            const CallInst* call_inst = dyn_cast<CallInst>(&I);
            if (call_inst == nullptr) continue;
            if (call_inst->getCalledFunction() != func) continue;
            auto leaked_arg_idxs = func_info.leaked_arg_idxs;
            for (auto idx : leaked_arg_idxs) {
                auto arg = call_inst->getArgOperand(idx);
                for (auto E : get_output_edges(*BB_mapped)) {
                    leakage_info[E].insert(arg);
                    leaked_values.insert(arg);
                }
            }
        }
    }
}

void get_knowledge_frontier(Function* outer_func,
                            EdgeLevelAnalysis& initial_leakage,
                            KnowledgeFrontierMap& frontier_map,
                            ValueSet& original_values,
                            ValueTreeMap& value_tree)
{
    CFGEdgeSet all_edges;
    for (auto& BB: *outer_func) {
        for (auto BBNext : successors(&BB)) {
            all_edges.insert(make_pair(&BB, BBNext));
        }
        for (auto BBPrev : predecessors(&BB)) {
            all_edges.insert(make_pair(BBPrev, &BB));
        }
    }

    EdgeLevelAnalysis quasi_ELA;
    for (auto pair : initial_leakage) {
        quasi_ELA[pair.first] = {};
        for (auto v : pair.second) {
            quasi_ELA[pair.first].insert(v);
        }
    }

    bool work_done = true;
    while (work_done) {
        bool intra_edge_did_work = intra_edge(quasi_ELA, *outer_func);
        bool inter_edge_did_work = inter_edge(quasi_ELA, *outer_func);
        bool phi_prop_did_work = phi_prop(quasi_ELA, *outer_func);
        work_done = intra_edge_did_work || inter_edge_did_work
            || phi_prop_did_work;
    }

    knowledge_expansion(quasi_ELA, *outer_func);

    BlockLevelAnalysis quasi_BLA;
    reduce(quasi_ELA, *outer_func, quasi_BLA);
    loop_map(quasi_BLA, *outer_func, value_tree, original_values);
    loop_reduce(quasi_BLA, *outer_func, original_values);

    frontier_map = knowledge_frontier(*outer_func, quasi_BLA);
}

int place_barriers_optimally(Function* outer_func,
                             const unordered_set<const Function*> called_funcs,
                             const unordered_map<const Function*, BlockSet> call_sites,
                             const unordered_map<const Function*, FunctionInfo>& function_info_map,
                             BlockSet& barrier_locs,
                             ValueToValueMapTy& VMapToNonSLHOuterFunc)
{
    ValueToValueMapTy VMapToExpanded;
    auto outer_func_clone = CloneFunction(outer_func, VMapToExpanded);
    unordered_map<const BasicBlock*, const BasicBlock*> ReverseVMap;
    for (auto& BB_original : *outer_func) {
        auto BB_new = dyn_cast<BasicBlock>(VMapToExpanded[&BB_original]);
        ReverseVMap[BB_new] = &BB_original;
    }

    LoopInfo cfg_loops = get_all_loops(*outer_func_clone);
    DefUseMap DUM = get_outgoing_values(*outer_func_clone, cfg_loops);
    ValueSet original_values = get_original_values(*outer_func_clone);
    ValueTreeMap value_tree = expand_all_loops(*outer_func_clone, DUM);

    EdgeLevelAnalysis initial_leakage;
    ValueSet leaked_vals;
    for (auto E : get_all_edges(*outer_func_clone)) {
        initial_leakage[E] = {};
    }
    for (auto inner_func : called_funcs) {
        vals_leaked_by_func_calls(inner_func, function_info_map.at(inner_func),
                                  call_sites.at(inner_func), initial_leakage,
                                  VMapToExpanded, leaked_vals);
    }

    KnowledgeFrontierMap frontier_map;
    get_knowledge_frontier(outer_func_clone, initial_leakage, frontier_map,
                           original_values, value_tree);

    int num_barriers_added = 0;

    for (auto leaked_val : leaked_vals) {
        auto frontier = frontier_map[leaked_val];
        for (auto BB : frontier) {
            auto BB_from_non_expanded = ReverseVMap[BB];
            auto target_BB = dyn_cast<BasicBlock>(VMapToNonSLHOuterFunc[BB_from_non_expanded]);
            if (!barrier_locs.contains(target_BB)) {
                barrier_locs.insert(target_BB);
                num_barriers_added++;
            }
        }
    }

    outer_func_clone->eraseFromParent();

    return num_barriers_added;
}

enum class RelaxationStatus {
    CallerEnf,
    CalleeEnf,
    Degenerate, // TODO: "Degenerate" should be renamed to "Trivial" as per the paper's terminology
    InheritedCalleeEnf
};

void relax_function(Function* F,
                    const unordered_map<const Function*, FunctionInfo>& function_info_map,
                    const KnowledgeFrontierMap& frontier_map,
                    unordered_map<const Function*, const Function*>& non_SLH_mappings,
                    const unordered_set<const Function*>& called_funcs,
                    const unordered_map<const Function*, BlockSet>& call_sites,
                    unordered_map<const Function*, RelaxationStatus>& relaxation_statuses)
{
    FunctionInfo func_info = function_info_map.at(F);

    errs() << "Relaxing " << to_string(F) << "\n";

    BlockSet barrier_locs;

    ValueToValueMapTy VMap;
    auto F_NoSLH = CloneFunction(F, VMap);
    F_NoSLH->setName(F->getName() + "_NoSLH");

    // TODO: compactify this with a union operation
    BlockSet frontier_of_transmitted_vals;
    for (auto V : func_info.transmitted_vals) {
        BlockSet frontier = frontier_map.at(V);
        for (auto BB : frontier) {
            assert(VMap[BB]);
            auto target_BB = dyn_cast<BasicBlock>(VMap[BB]);
            frontier_of_transmitted_vals.insert(target_BB);
        }
    }

    bool calls_relaxed_functions = false;
    bool need_to_enforce_callees = false;
    for (auto called_func : called_funcs) {
        auto callee_relaxation_status = relaxation_statuses[called_func];
        if (callee_relaxation_status != RelaxationStatus::Degenerate) {
            calls_relaxed_functions = true;
        }
        if (callee_relaxation_status == RelaxationStatus::CallerEnf) {
            need_to_enforce_callees = true;
        }
    }

    if (func_info.degenerate_case && !calls_relaxed_functions) {
        F_NoSLH->removeFromParent();
        relaxation_statuses[F] = RelaxationStatus::Degenerate;
        errs() << "degenerate case, skipping\n";
        errs() << "-------\n";
        return;
    }

    // IMPORTANT: If you don't want to remove the SLH attribute for the
    // relaxed function becuase you want to keep inter-function predicate
    // state propagation, then DON'T remove the Fn Attr and instead use the
    // -x86-slh-loads=0 compiler option to disable load hardening. Note
    // that this will apply  to all other functions in the source file.
    // As long as relaxed functions only call other relaxed functions (or
    // denegerate functions), there's no security flaw.
    F_NoSLH->removeFnAttr(Attribute::SpeculativeLoadHardening);
    non_SLH_mappings[F] = F_NoSLH;

    if (calls_relaxed_functions && func_info.degenerate_case && !func_info.pure_transmitter) {
        relaxation_statuses[F] = RelaxationStatus::InheritedCalleeEnf;
        F_NoSLH->setName(F_NoSLH->getName() + "_CalleeEnf");
        errs() << "inherited callee enforcement from its callees\n";
    }

    if (!func_info.degenerate_case && !func_info.pure_transmitter) {
        relaxation_statuses[F] = RelaxationStatus::CalleeEnf;
        F_NoSLH->setName(F_NoSLH->getName() + "_CalleeEnf");

        int num_barriers_added = 0;
        for (auto BB : frontier_of_transmitted_vals) {
            if (!barrier_locs.contains(BB)) {
                barrier_locs.insert(BB);
                num_barriers_added++;
            }
        }

        assert(num_barriers_added > 0);

        errs() << "callee enforced, "
               << num_barriers_added << " barrier"
               << (num_barriers_added != 1 ? "s" : "")
               << " added for variables\n";
    }

    if (func_info.pure_transmitter) {
        relaxation_statuses[F] = RelaxationStatus::CallerEnf;
        F_NoSLH->setName(F_NoSLH->getName() + "_CallerEnf");
        errs() << "! caller enforced, no barriers added for variables\n";
    }

    if (need_to_enforce_callees) {
        unordered_set<const Function*> funcs_to_protect;
        for (auto called_func : called_funcs) {
            if (relaxation_statuses[called_func] == RelaxationStatus::CallerEnf) {
                funcs_to_protect.insert(called_func);
            }
        }

        BlockSet dummy_barrier_locs;
        int num_barriers_added = place_barriers_optimally(F, funcs_to_protect, call_sites,
                                                          function_info_map, dummy_barrier_locs, VMap);

        // TODO: This shouldn't happen if F is a top-level function. It's not wrong, just not that clean.
        auto target_entry_BB = dyn_cast<BasicBlock>(VMap[&F->getEntryBlock()]);
        if (!barrier_locs.contains(target_entry_BB) || !func_info.pure_transmitter) {
            for(auto BB : dummy_barrier_locs) {
                barrier_locs.insert(BB);
            }

            if (num_barriers_added > 0) {
                errs() << num_barriers_added << " barrier"
                    << (num_barriers_added != 1 ? "s" : "")
                    << " added for callees\n";
            }
        }
    }

    for (auto BB : barrier_locs) {
        auto BB_non_const = const_cast<BasicBlock*>(BB);
        add_specbarr_to_block(*BB_non_const);
        errs() << "  " << to_string(BB_non_const) << "\n";
    }

    if(calls_relaxed_functions) {
        vector<tuple<const Function*, const BasicBlock*, const Function*>> replacements;

        for (auto called_func : called_funcs) {
            if (!non_SLH_mappings.contains(called_func)) continue;
            auto relaxed_func = non_SLH_mappings[called_func];
            if (relaxed_func == nullptr) continue;
            for (auto& BB : *F_NoSLH) {
                for (auto& I : BB) {
                    const CallInst* call_inst = dyn_cast<CallInst>(&I);
                    if (call_inst == nullptr) continue;
                    if (call_inst->getCalledFunction() != called_func) continue;
                    replacements.push_back(make_tuple(called_func, &BB, relaxed_func));
                    const_cast<CallInst*>(call_inst)->setCalledFunction(const_cast<Function*>(relaxed_func));
                }
            }
        }

        int num_repls = replacements.size();
        if (num_repls > 0) {
            errs() << "replacing function calls with softened counterparts "
                << "(" << num_repls << " call" << (num_repls != 1 ? "s" : "") << " replaced)\n";
            for (auto tup : replacements) {
                errs() << "  in " << to_string(get<1>(tup)) << ": " << to_string(get<0>(tup))
                       << " -> " << to_string(get<2>(tup)) << "\n";
            }
        }
    }

    errs() << "-------\n";
}

PreservedAnalyses Relaxation::run(Module &M, ModuleAnalysisManager &) {
    if (InputFilename.empty()) {
        errs() << "ERROR: YAML input file is required!\n";
        exit(1);
    }

    string input_file_name = InputFilename;
    fs::path input_file_path(input_file_name);
    if (!fs::exists(input_file_path)) {
        errs() << "ERROR: Provided input file doesn't exist!\n";
        exit(1);
    }

    if (OutputFolder == "") {
        errs() << "ERROR: An output folder MUST be provided!\n";
        exit(1);
    }

    string output_folder_name = OutputFolder;
    fs::path output_folder_path(output_folder_name);
    string module_name = to_string(&M);
    output_folder_path = (output_folder_path / module_name).lexically_normal();
    fs::path output_file_path = output_folder_path / "relaxed.ll";
    if (!fs::exists(output_folder_path)) {
        errs() << "Provided output folder doesn't exist, creating it\n";
        fs::create_directories(output_folder_path);
    }

    YAML::Node yroot;
    try {
        yroot = YAML::LoadFile(input_file_path);
    }
    catch(...) { // TODO: forward the error message from yaml-cpp (YAML::ParserException)
        errs() << "ERROR: Could not open provded YAML file!\n";
        exit(1);
    }

    unordered_set<Function*> user_functions;
    for (auto& F : M.functions()) {
        if (is_user_function(F)) {
            user_functions.insert(&F);
        }
    }

    unordered_map<const Function*, unordered_map<const Function*, BlockSet>> call_loc_matrix;
    unordered_map<const Function*, unordered_set<const Function*>> call_map;
    for (auto F1 : user_functions) {
        call_map[F1] = {};
        for (auto F2 : user_functions) {
            call_loc_matrix[F1][F2] = {};
        }
    }

    unordered_map<string, Function*> names_to_funcs;
    unordered_map<string, BasicBlock*> names_to_blocks;
    unordered_map<const Function*, unordered_map<string, Value*>> names_to_values_per_func;
    for (auto F : user_functions) {
        names_to_funcs[to_string(F)] = F;
        for (auto& arg : F->args()) {
            names_to_values_per_func[F][to_string(&arg)] = &arg;
        }
        for (auto& BB : *F) {
            names_to_blocks[to_string(&BB)] = &BB;
            for (auto& I : BB) {
                names_to_values_per_func[F][to_string(&I)] = &I;
                CallInst* func_call_inst = dyn_cast<CallInst>(&I);
                if(func_call_inst != nullptr) {
                    Function* called_func = func_call_inst->getCalledFunction();
                    if (user_functions.contains(called_func)) {
                        call_map[F].insert(called_func);
                        call_loc_matrix[F][called_func].insert(&BB);
                    }
                }
            }
        }
    }

    KnowledgeFrontierMap frontier_map;
    // TODO: typedef this one FunctionInfo becomes a utility class
    unordered_map<const Function*, FunctionInfo> function_info_map;
    for(auto function_node : yroot) {
        auto function_name = function_node.begin()->first.as<string>();
        auto F = names_to_funcs[function_name];
        auto function_data = function_node.begin()->second;
        for(auto phase_data : function_data) {
            auto phase_name = phase_data["phase_name"].as<string>();
            if (phase_name == "knowledge_frontier") {
                auto all_states = phase_data["state"];
                for (auto state : all_states){
                    auto var_name = state.first.as<string>();
                    assert(names_to_values_per_func[F].contains(var_name));
                    auto V = names_to_values_per_func[F][var_name];
                    auto blocks = state.second;
                    for (auto block : blocks) {
                        auto block_name = block.as<string>();
                        auto BB = names_to_blocks[block_name];
                        frontier_map[V].insert(BB);
                    }
                }
            }
            else if (phase_name == "summary") {
                auto F = names_to_funcs[function_name];
                function_info_map[F] = {};
                for (auto kv : phase_data) {
                    auto data_name = kv.first.as<string>();
                    auto data = kv.second;
                    if (data_name == "transmitted_vals") {
                        for (auto val : data) {
                            auto val_name = val.as<string>();
                            auto V = names_to_values_per_func[F][val_name];
                            function_info_map[F].transmitted_vals.insert(V);
                        }
                    }
                    else if (data_name == "leaked_arg_idxs") {
                        for (auto arg_idx : data) {
                            function_info_map[F].leaked_arg_idxs.insert(arg_idx.as<int>());
                        }
                    }
                    else if (data_name == "pure_transmitter") {
                        function_info_map[F].pure_transmitter = data.as<bool>();
                    }
                    else if (data_name == "degenerate_case") {
                        function_info_map[F].degenerate_case = data.as<bool>();
                    }
                }
            }
        }
    }

    // Pretty sure we won't need this
    CallGraph CG(M);
    CG.getModule(); // For some reason the code crashes unless I do
                    // SOMETHING with the callgraph...
    unordered_set<const Function*> analyzed_functions;
    unordered_map<const Function*, const Function*> non_SLH_mappings;
    unordered_map<const Function*, RelaxationStatus> relaxation_statuses;
    bool work_done = true;
    while (work_done) {
        work_done = false;
        for (auto F : user_functions) {
            if (analyzed_functions.contains(F)) continue;
            bool ready = true;
            for (auto G : get_called_funcs(CG, F)) {
                ready = ready && analyzed_functions.contains(G);
            }
            if (ready) {
                relax_function(F, function_info_map, frontier_map, non_SLH_mappings,
                               call_map[F], call_loc_matrix[F], relaxation_statuses);
                analyzed_functions.insert(F);
                work_done = true;
            }
        }
    }

    ofstream relaxed_output_file(output_file_path);
    string relaxed_code;
    raw_string_ostream sstr(relaxed_code);
    sstr << M;
    sstr.flush();
    relaxed_output_file << relaxed_code;
    relaxed_output_file.close();

    ofstream relaxed_functions_file(output_folder_path / "functions.txt");
    for (auto F : user_functions) {
        auto F_NoSLH = non_SLH_mappings[F];
        if (F_NoSLH != nullptr) {
            relaxed_functions_file << to_string(F_NoSLH) << "\n";
        }
    }
    relaxed_functions_file.close();

    return PreservedAnalyses::all();
}


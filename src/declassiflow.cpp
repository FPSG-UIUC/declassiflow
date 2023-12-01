#include "declassiflow.h"
#include "flagging.h"
#include "relaxation.h"
#include "passes.h"
#include "cleanup.h"
#include "ANSI.h"
#include "loop.h"

#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Analysis/CallGraph.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/Support/CommandLine.h"

#include <unordered_map>
#include <set>
#include <tuple>
#include <string>
#include <sstream>
#include <algorithm>
#include <filesystem>

#include "yaml-cpp/yaml.h"

// TODO: This should be renamed to the "dataflow" pass since
// "Declassiflow" refers to the entire process

// TODO: Everything printed to errs() should be printed to outs()
// but only if we're in debug mode, and everything originally printed
// to outs() should be directly written to a YAML file
#define DEBUG_TYPE "declassiflow"

using namespace llvm;
using namespace std;
namespace fs = std::filesystem;

cl::opt<string> InputFilename("input",
                              cl::desc("Path to YAML file used by analysis"),
                              cl::value_desc("filename"));

void runDFAOnFunction(Function &F,
                      InterFunctionLeakage& inter_function_leakage,
                      YAML::Emitter& yout)
{
    errs() << "Analyzing " << to_string(&F) << "\n";

    int num_blocks = 0, num_edges = 0, num_instrs = 0;
    for (auto& BB : F) {
        num_blocks++;
        for (auto& I : BB) {
            num_instrs++;
        }
    }
    for (auto edge : get_all_edges(F)) num_edges++;
    errs() << "Num blocks: " << num_blocks << "\n";
    errs() << "Num edges: " << num_edges << "\n";
    errs() << "Num instrs: " << num_instrs << "\n";

    yout << YAML::BeginMap
         << YAML::Key << F.getName().str()
         << YAML::Value
         << YAML::BeginSeq;

    bool valid_loops = loop_condition_check(F);
    if(valid_loops == false) {
        errs() << "Function @" << F.getName() << " failed simple loop check\n";
        errs() << "  Skipping analysis\n";
        return;
    }

    LoopInfo cfg_loops = get_all_loops(F);
    DefUseMap DUM = get_outgoing_values(F, cfg_loops);
    ValueSet original_values = get_original_values(F);
    ValueTreeMap value_tree = expand_all_loops(F, DUM);

    EdgeLevelAnalysis ELA;
    for (auto edge : get_all_edges(F)) {
        ELA[edge] = {};
    }

    unsigned int pass_counter = 1;

    // Read in YAML file
    // TODO: This fill reading and checking should really happen in
    // the outer function, not each time the DFA is run
    bool initialization_pass_did_work = false;
    if (!InputFilename.empty()) {
        string input_file_name = InputFilename;
        fs::path input_file_path(input_file_name);
        if (!fs::exists(input_file_path)) {
            errs() << "ERROR: Provided input file doesn't exist!\n";
            exit(1);
        }
        try {
            YAML::Node config = YAML::LoadFile(InputFilename);
            initialization_pass_did_work = append_YAML_to_ELA(F, config, ELA);
            if (initialization_pass_did_work) {
                serialize_ELA(yout, "initialization", ELA, pass_counter);
                pass_counter++;
            }
        }
        catch(...) { // TODO: forward the error message from yaml-cpp (YAML::ParserException)
            errs() << "ERROR: Could not open provded YAML file!\n";
            exit(1);
        }
    } else {
        errs() << "No YAML file provided, skipping initialization\n";
    }

    // TODO: finding transmitters and finding stores should be wrapped into an "init" pass
    ValueSet locally_transmitted_vals;
    ValueSet all_transmitted_vals;
    unordered_map<const Value*, BlockSet> transmitter_locs;
    bool transmit_pass_did_work = find_transmits(ELA, F, locally_transmitted_vals, transmitter_locs);
    for(BasicBlock& BB : F) {
        for(Instruction& I : BB) {
            auto Store = dyn_cast<StoreInst>(&I);
            if (Store != nullptr) {
                for (auto E : get_output_edges(BB)) {
                    ELA[E].insert(Store->getPointerOperand());
                }
            }
        }
    }

    if (transmit_pass_did_work) {
        serialize_ELA(yout, "transmit", ELA, pass_counter);
        pass_counter++;
    }

    // Inter function leakage (search for all call instructions and run interfunctionleakage class on it).
    // Want it after directly leaked values are recorded as these aren't directly leaked!!!!
    bool interfunction_did_work = false;
    for(BasicBlock& BB : F) {
        for(Instruction& I : BB) {
            CallInst* func_call_inst = dyn_cast<CallInst>(&I);
            if(func_call_inst != nullptr) {
                Function* called_func = func_call_inst->getCalledFunction();
                if (!is_user_function(*called_func)) continue;

                ValueSet leaked_args = inter_function_leakage.get_arg_leakage(called_func,
                                                                              func_call_inst);
                all_transmitted_vals.insert(leaked_args.begin(), leaked_args.end());

                for(auto EOut : get_output_edges(BB)) {
                    int old_size = ELA[EOut].size();
                    ELA[EOut].insert(leaked_args.begin(), leaked_args.end());
                    int new_size = ELA[EOut].size();
                    if (new_size > old_size) {
                        interfunction_did_work = true;
                    }
                }
            }
        }
    }
    if (interfunction_did_work) {
        // serialize_ELA(yout, "interfunction_leakage", ELA, pass_counter);
        pass_counter++;
    }

    bool work_done = true;
    while(work_done) {
        bool intra_edge_did_work = intra_edge(ELA, F);
        if (intra_edge_did_work) {
            serialize_ELA(yout, "intra_edge", ELA, pass_counter);
            pass_counter++;
        }
        bool inter_edge_did_work = inter_edge(ELA, F);
        if (inter_edge_did_work) {
            serialize_ELA(yout, "inter_edge", ELA, pass_counter);
            pass_counter++;
        }
        bool phi_prop_did_work = phi_prop(ELA, F);
        if (phi_prop_did_work) {
            serialize_ELA(yout, "phi_prop", ELA, pass_counter);
            pass_counter++;
        }
        work_done = intra_edge_did_work || inter_edge_did_work
          || phi_prop_did_work;
    }

    // clean up passes
    BlockLevelAnalysis BLA;
    reduce(ELA, F, BLA);
    loop_map(BLA, F, value_tree, original_values);
    loop_reduce(BLA, F, original_values);
    // TODO: We ought to restore the original CFG before serializing for nicer outputs
    serialize_BLA(yout, "reduced", BLA, pass_counter);
    pass_counter += 3;

    // grab knowledge frontier
    KnowledgeFrontierMap frontier_map = knowledge_frontier(F, BLA);
    serialize_knowledge_frontier(yout, "knowledge_frontier", frontier_map, pass_counter);
    pass_counter++;

    // TODO: The remainder of the code conflates calculation, printing,
    // and serializing to YAML. These should be pulled apart!

    yout << YAML::BeginMap
         << YAML::Key << "phase_name"
         << YAML::Value << "summary";

    yout << YAML::Key << "transmitted_vals"
         << YAML::Value << YAML::Flow << YAML::BeginSeq;
    if (!locally_transmitted_vals.empty()) {
        errs() << "locally transmitted values:\n";
    }
    bool fully_declassified = true;
    auto entry_leaked_vals = BLA[&(F.getEntryBlock())];
    for(auto transmitted_val : locally_transmitted_vals) {
        if(!original_values.contains(transmitted_val)) continue;
        errs() << "  " << to_string(transmitted_val) << "  ";
        yout << to_string(transmitted_val);
        if (entry_leaked_vals.contains(transmitted_val)) {
            errs() << "(fully declassified)\n";
        }
        else {
            errs() << "(partially declassified)\n";
            fully_declassified = false;
        }
    }
    yout << YAML::EndSeq;

    if (fully_declassified) {
        errs() << "function is fully declassified\n";
    }
    else {
        errs() << "function is NOT fully declassified\n";
    }

    yout << YAML::Key << "leaked_arg_idxs"
         << YAML::Value << YAML::Flow << YAML::BeginSeq;
    ValueSet leaked_args;
    for (int arg_idx = 0; arg_idx < F.arg_size(); arg_idx++) {
        auto arg = F.getArg(arg_idx);
        if (entry_leaked_vals.contains(arg)) {
            inter_function_leakage.add_arg_leakage(&F, arg);
            leaked_args.insert(arg);
            yout << arg_idx;
        }
    }
    yout << YAML::EndSeq;

    bool is_pure_transmitter = false; // TODO: Rename this to "pseudo transmitter" to match the paper's terminology
    if (!leaked_args.empty()) {
        errs() << "leaked arguments:\n";
        int arg_idx = 0;
        bool args_fully_declassified = true;
        for (auto leaked_arg : leaked_args) {
            errs() << "  " << to_string(leaked_arg)
                   << " (arg " << arg_idx++ << ") ";
            if (entry_leaked_vals.contains(leaked_arg)) {
                errs() << "(fully declassified)\n";
            }
            else {
                errs() << "(partially declassified)\n";
                args_fully_declassified = false;
            }
        }

        if (fully_declassified && args_fully_declassified) {
            // Do a quasi-DFA
            EdgeLevelAnalysis quasi_ELA;
            for (auto E : get_all_edges(F)) {
                quasi_ELA[E] = {};
            }
            for (auto E : get_output_edges(F.getEntryBlock())) {
                quasi_ELA[E] = leaked_args;
            }
            // TODO: I'm assuming inter-edge prop. wouldn't help in this situation.
            // I need to think about whether that's really the case...
            bool work_done = true;
            while (work_done) {
                work_done = intra_edge(quasi_ELA, F);
                BlockLevelAnalysis tmp;
                reduce(quasi_ELA, F, tmp);
            }
            knowledge_expansion(quasi_ELA, F);
            BlockLevelAnalysis quasi_BLA;
            reduce(quasi_ELA, F, quasi_BLA);
            // TODO: Use a subset method for this eventually
            bool is_subset = true;
            for (auto transmitted_val : all_transmitted_vals) {
                if (!quasi_BLA[&F.getEntryBlock()].contains(transmitted_val)) {
                    is_subset = false;
                    errs() << "! " << to_string(transmitted_val) << "\n";
                    break;
                }
            }
            is_pure_transmitter = is_subset;
        }
    }

    bool degenerate_case = true; // TODO: Rename this to "trivial case" to match the paper's terminology
    for (auto transmitted_val : locally_transmitted_vals) {
        // TODO: Skipping globals for now, but we should really think
        // about where they fit in our analysis
        if (isa<GlobalValue>(transmitted_val)) continue;
        BlockSet transmitter_blocks = transmitter_locs[transmitted_val];
        BlockSet frontier = frontier_map[transmitted_val];
        // TODO: should replace this with a call to a generic equality function
        for (auto BB : transmitter_blocks) {
            if (!frontier.contains(BB)) degenerate_case = false;
        }
        for (auto BB : frontier) {
            if (!transmitter_blocks.contains(BB)) degenerate_case = false;
        }
    }

    if (is_pure_transmitter) {
        errs() << "function is a pure transmitter\n";
    }
    else {
        errs() << "function is NOT a pure transmitter\n";
    }

    yout << YAML::Key << "fully_declassified"
         << YAML::Value << fully_declassified;
    yout << YAML::Key << "pure_transmitter"
         << YAML::Value << is_pure_transmitter;
    yout << YAML::Key << "degenerate_case"
         << YAML::Value << degenerate_case;

    errs() << "-------\n";

    yout << YAML::EndMap;

    yout << YAML::EndSeq
         << YAML::EndMap;
}


// TODO: make this a utility function
vector<Function*> get_called_funcs(const CallGraph& CG,
                                   const Function* Caller)
{
    vector<Function*> called_funcs;
    auto CGNode = CG[Caller];
    for (auto it = CGNode->begin(); it != CGNode->end(); it++) {
        auto Called = it->second->getFunction();
        if (Called && is_user_function(*Called)) {
            called_funcs.push_back(Called);
        };
    }
    return called_funcs;
}


PreservedAnalyses Declassiflow::run(Module &M, ModuleAnalysisManager &) {
    vector<Function*> user_functions;

    for (auto& F : M.functions()) {
        if (is_user_function(F)) {
            user_functions.push_back(&F);
        }
    }

    YAML::Emitter yout;
    yout << YAML::BeginSeq;


    InterFunctionLeakage inter_function_leakage(&M);

    CallGraph CG(M);
    CG.getModule(); // For some reason the code crashes unless I do
                    // SOMETHING with the callgraph...
    unordered_set<Function*> analyzed_functions;
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
                runDFAOnFunction(*F, inter_function_leakage, yout);
                analyzed_functions.insert(F);
                work_done = true;
            }
        }
    }

    yout << YAML::EndSeq;
    outs() << yout.c_str() << "\n";

    return PreservedAnalyses::all();
}

// InterFunctionLeakage class defintions
InterFunctionLeakage::InterFunctionLeakage(Module* M) {
    this->M = M;
}

void InterFunctionLeakage::add_arg_leakage(const Function* F, const Value* leaked) {
    if(!function_to_arg_leakage.contains(F)) {
        function_to_arg_leakage[F] = vector<int>(0, 0);
    }
    int i = 0;
    for(auto& arg : F->args()) {
        if(leaked == dyn_cast<Value>(&arg)) {
            function_to_arg_leakage[F].push_back(i);
            return;
        }
        i++;
    }
    // LEAKED FUNC ARG WASN'T ACTUALLY FUNC ARG, THIS IS A PROBLEM!
    assert(false);
}

ValueSet InterFunctionLeakage::get_arg_leakage(const Function* F, const CallInst* I) {
    if(!function_to_arg_leakage.contains(F)) return {};
    ValueSet ret;
    vector<int> index_leaked = function_to_arg_leakage[F];
    for(int i : index_leaked) {
        ret.insert(I->getArgOperand(i));
    }
    return ret;
}


//-----------------------------------------------------------------------------
// New PM Registration
//-----------------------------------------------------------------------------

// TODO: The "declassiflow" pass should be renamed to the "dataflow" pass since
// Declassiflow consists of all three passes. The data-flow pass should be a
// separate source file.

PassPluginLibraryInfo getDeclassiflowPluginInfo() {
    return {LLVM_PLUGIN_API_VERSION, "declassiflow", LLVM_VERSION_STRING,
            [](PassBuilder& PB) {
                PB.registerPipelineParsingCallback(
                    [](StringRef Name, ModulePassManager &MPM,
                       ArrayRef<PassBuilder::PipelineElement>) {
                        if (Name == "declassiflow") {
                            MPM.addPass(Declassiflow());
                            return true;
                        }
                        return false;
                    });
                PB.registerPipelineParsingCallback(
                    [](StringRef Name, FunctionPassManager& FPM,
                       ArrayRef<PassBuilder::PipelineElement>) {
                        if (Name == "flagging") {
                            FPM.addPass(Flagging());
                            return true;
                        }
                        return false;
                    });
                PB.registerPipelineParsingCallback(
                    [](StringRef Name, ModulePassManager& MPM,
                       ArrayRef<PassBuilder::PipelineElement>) {
                        if (Name == "relaxation") {
                            MPM.addPass(Relaxation());
                            return true;
                        }
                        return false;
                    });
            }};
}

extern "C" LLVM_ATTRIBUTE_WEAK ::PassPluginLibraryInfo
llvmGetPassPluginInfo() {
    return getDeclassiflowPluginInfo();
}

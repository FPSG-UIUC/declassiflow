#include "util.h"
#include "ANSI.h"

#include "llvm/Support/Debug.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Analysis/TargetLibraryInfo.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/InstIterator.h"

#include <queue>
#include <iterator>

using namespace llvm;
using namespace std;

// doesn't quite return a traditional BLA with values but with the string names of those values
// https://stackoverflow.com/questions/365155/parse-yaml-files-in-c
PseudoBlockLevelAnalysis pseudo_BLA_from_YAML(Function& F, YAML::Node& s) {
    PseudoBlockLevelAnalysis pseudo_BLA;

    for(auto function_node : s) {
        auto function_name = function_node.begin()->first.as<string>();
        if (function_name != F.getName().str()) continue;
        auto function_data = function_node.begin()->second;
        for(auto phase_data : function_data) {
            auto phase_name = phase_data["phase_name"].as<string>();
            if (phase_name != "reduced") continue;
            auto all_states = phase_data["state"];
            for (auto state : all_states){
                auto block_name = state.first.as<string>();
                auto decl_vars = state.second;
                for (auto var : decl_vars) {
                    auto var_name = var.as<string>();
                    pseudo_BLA[block_name].push_back(var_name);
                }
            }
        }
    }

    return pseudo_BLA;
}


bool append_YAML_to_ELA(Function& F, YAML::Node& s, EdgeLevelAnalysis& ELA) {
    // Get declassified variables from YAML
    unordered_set<string> declassified_block_names;
    unordered_set<string> declassified_value_names;
    unordered_map<string, unordered_set<string>> klee_results;
    for(auto function_node : s) {
        auto function_name = function_node.begin()->first.as<string>();
        if (function_name != F.getName().str()) continue;
        auto function_data = function_node.begin()->second;
        auto block_name = function_data.begin()->first.as<string>();
        declassified_block_names.insert(block_name);
        auto vars = function_data.begin()->second;
        unordered_set<string> declassified_vars;
        for (auto var : vars) {
            auto var_name = var.as<string>();
            declassified_value_names.insert(var_name);
            declassified_vars.insert(var_name);
        }
        klee_results[block_name] = declassified_vars;
     }

    // Get locations of all definitions so that we don't declassify
    // higher in the CFG than is sound
    DominatorTree dom_tree = DominatorTree(F);
    unordered_map<const Value*, BasicBlock*> defn_locs;
    for (inst_iterator it = inst_begin(F), it_end = inst_end(F); it != it_end; ++it) {
        auto& I = *it;
        auto* BB = I.getParent();
        defn_locs[&I] = BB;
    }

    // We need to get the instructions that correspond to the
    // names from the YAML
    unordered_map<string, Value*> names_to_values;
    for (inst_iterator it = inst_begin(F), it_end = inst_end(F); it != it_end; ++it) {
        auto* I = &*it;
        names_to_values[to_string(I)] = I;
    }

    for (auto& Arg : F.args()) {
        names_to_values[to_string((Value *)&Arg)] = (Value*)&Arg;
    }

    // Add them to the edge-level analysis
    bool did_work = false;
    for (auto& BB : F) {
        auto block_name = to_string(&BB);
        if (!declassified_block_names.contains(block_name)) continue;
        ValueSet values_to_declassify;
        auto declassified_value_names = klee_results[block_name];
        for (auto value_name : declassified_value_names) {
            auto V = names_to_values[value_name];
            if (defn_locs.contains(V) && !dom_tree.dominates(defn_locs[V], &BB)) {
                continue;
            }
            for (auto EOut : get_output_edges(BB)) {
                auto& DFV = ELA[EOut];
                if (!DFV.contains(V)) {
                    DFV.insert(V);
                    did_work = true;
                }
            }
        }
    }

    return did_work;
}


bool is_access(const Instruction* I) {
    return isa<LoadInst>(I);
}


bool is_transmit(const Instruction* I) {
    return isa<LoadInst>(I);
}


bool is_dataflow_instr(const Instruction* I) {
    return !(I->getType()->isVoidTy() ||
             isa<LoadInst>(I) ||
             isa<CallInst>(I) ||
             isa<AllocaInst>(I));
}


bool is_const_val(const Value* V) {
    return isa<Constant>(V) && !isa<UndefValue>(V);
}


const Value* leaked_op(const Instruction& I) {
    assert(is_transmit(&I) && "Attempting to get leaked op of a non-transmit instruction");
    auto Load = dyn_cast<LoadInst>(&I);
    if (Load != nullptr) {
        return Load->getPointerOperand();
    }
    else {
        return nullptr; // WILL NOT HAPPEN!
    }
}


string get_instr_dest(const Instruction* I) {
    string str;
    raw_string_ostream sstr(str);
    sstr << *I;
    istringstream input(str);
    string dest;
    input >> dest;
    return dest;
}


string get_arg_dest(const Argument* A) {
    string str;
    raw_string_ostream sstr(str);
    sstr << *A;
    istringstream input(str);
    string dest;
    input >> dest;
    input >> dest;
    return dest;
}


bool is_user_function(const Function& F) {
    const TargetLibraryInfo *TLI;
    LibFunc inbuilt_func;
    if(TLI == 0 || *(uint64_t*)TLI == 0) return false; // For some reason printf's seem to
    return !TLI->getLibFunc(F, inbuilt_func)           // be why *TLI == 0
                && !F.empty();
}


string get_bb_dest(const BasicBlock* BB) {
    auto V = dyn_cast<Value>(BB);
    string str;
    raw_string_ostream sstr(str);
    sstr << *V;
    istringstream input(str);
    string dest;
    input >> dest;
    return dest;
}


string to_string(const Value* V) {
    if (V == nullptr) return "NULL";
    string str;
    raw_string_ostream ss(str);
    if (auto BB = dyn_cast<BasicBlock>(V)) {
        string basic_block_str = get_bb_dest(BB);
        if(basic_block_str[0] == '%')
            basic_block_str = basic_block_str.substr(1, basic_block_str.size()-1);
        if(basic_block_str[basic_block_str.size()-1] == ':')
            basic_block_str = basic_block_str.substr(0, basic_block_str.size()-1);
        ss << BB->getParent()->getName() << "::" << basic_block_str;
    }
    else if (auto F = dyn_cast<Function>(V)) {
        ss << F->getName();
    }
    else if (auto I = dyn_cast<Instruction>(V)) {
        ss << get_instr_dest(I);
    }
    else if (auto A = dyn_cast<Argument>(V)) {
        ss << get_arg_dest(A);
    }
    else if (auto CInt = dyn_cast<ConstantInt>(V)) {
        ss << CInt->getValue();
    }
    else if (auto CFP = dyn_cast<ConstantFP>(V)) {
        SmallVector<char, 32> str;
        CFP->getValue().toString(str, 0, 0);
        ss << str;
    }
    else if (auto CExpr = dyn_cast<ConstantExpr>(V)) {
        ss << "<" << CExpr->getOpcodeName() << ">";
    }
    else if (auto G = dyn_cast<GlobalVariable>(V)) {
        ss << "@" << G->getName();
    }
    else {
        ss << "???";
    }
    return str;
}


string to_string(const Module* M) {
    if (M == nullptr) return "NULL";
    string module_name = M->getName().str();
    auto idx = module_name.find_last_of(".");
    module_name = module_name.substr(0, idx);
    replace(module_name.begin(), module_name.end(), '.', '_');
    return module_name;
}


string to_string(const CFGEdge* E) {
    if (!E) return "NULL";
    stringstream ss;
    ss << "(";
    if (E->first) ss << to_string(E->first);
    else ss << "NULL";
    ss << ",";
    ss << to_string(E->second);
    ss << ")";
    return ss.str();
}


string to_string(const DataFlowValue& DFV) {
    stringstream ss;
    ss << "{";
    bool first_elem = true;
    for (auto V : DFV) {
        if (first_elem) {
            first_elem = false;
        }
        else {
            ss << ", ";
        }
        ss << to_string(V);
    }
    ss << "}";
    return ss.str();
}


void intersection(const DataFlowValue& A, const DataFlowValue& B, DataFlowValue& result) {
    for (auto a : A) {
        if (B.contains(a)) {
            result.insert(a);
        }
    }
}


void difference(const DataFlowValue& A, const DataFlowValue& B, DataFlowValue& result) {
    for (auto a : A) {
        if (!B.contains(a)) {
            result.insert(a);
        }
    }
}


ValueSet value_tree_children_of(ValueTreeMap& value_tree, const Value* parent) {
    ValueSet ret;
    queue<const Value*> next;
    for(const Value* child : value_tree[parent]) next.push(child);
    while(!next.empty()) {
        const Value* cur = next.front(); next.pop();
        ret.insert(cur);
        for(const Value* child : value_tree[cur]) next.push(child);
    }
    return ret;
}


// TODO: This function does not handle dummy edges correctly
// DO NOT USE THIS FUNCTION UNTIL THEN!
void print_blocks_and_edges(const Function& F, raw_string_ostream& sstr) {
    string str_tmp;
    raw_string_ostream sstr_tmp(str_tmp);
    for (auto& BB : F) {
        sstr_tmp <<  BLK;
        sstr_tmp << to_string(&BB) << "\n";
        sstr_tmp << "-----\n";
        sstr <<  str_tmp;
        str_tmp = "";
        bool any_preds = false;
        sstr_tmp << "  Input edges:\n";
        for (auto* BBPrev : predecessors(&BB)) {
            CFGEdge EIn = make_pair(BBPrev, &BB);
            sstr_tmp << "    " << to_string(&EIn) << "\n";
            any_preds = true;
        }
        if (any_preds) {
            sstr <<  str_tmp;
        }
        str_tmp = "";
        bool any_succs = false;
        sstr_tmp << "  Output edges:\n";
        for (auto* BBNext : successors(&BB)) {
            CFGEdge EOut = make_pair(&BB, BBNext);
            sstr_tmp << "    " << to_string(&EOut) << "\n";
            any_succs = true;
        }
        if (any_succs) {
            sstr <<  str_tmp;
        }
        str_tmp = "";
        if (!any_preds && !any_succs) {
            CFGEdge EDummy = make_pair(nullptr, &BB);
            sstr_tmp << "  Dummy edges:\n";
            sstr_tmp << "    " << to_string(&EDummy) << "\n";
            sstr <<  str_tmp;
        }
        str_tmp = "";
        sstr_tmp << "-----\n\n";
    }
    sstr <<  str_tmp;
    str_tmp = "";
}


// TODO: This function does not handle dummy edges correctly
// DO NOT USE THIS FUNCTION UNTIL THEN!
void print_edge_level_analysis(const EdgeLevelAnalysis& ELA, const Function& F, raw_string_ostream& sstr) {
    string str_tmp;
    raw_string_ostream sstr_tmp(str_tmp);
    for (auto& BB : F) {
        sstr_tmp <<  BLK;
        sstr_tmp << to_string(&BB) << "\n";
        sstr_tmp << "-----\n";
        sstr << str_tmp;
        str_tmp = "";
        bool any_preds = false;
        sstr_tmp << "  Input edges:\n";
        for (auto* BBPrev : predecessors(&BB)) {
            CFGEdge EIn = make_pair(BBPrev, &BB);
            auto DFV = ELA.at(EIn);
            sstr_tmp << "    " << to_string(&EIn) << "\n";
            sstr_tmp << MAG << "      " << to_string(DFV) << "\n" << BLK;
            any_preds = true;
        }
        if (any_preds) {
            sstr << str_tmp;
        }
        str_tmp = "";
        bool any_succs = false;
        sstr_tmp << "  Output edges:\n";
        for (auto* BBNext : successors(&BB)) {
            CFGEdge EOut = make_pair(&BB, BBNext);
            auto DFV = ELA.at(EOut);
            sstr_tmp << "    " << to_string(&EOut) << "\n";
            sstr_tmp << MAG << "      " << to_string(DFV) << "\n" << BLK;
            any_succs = true;
        }
        if (any_succs) {
            sstr << str_tmp;
        }
        str_tmp = "";
        if (!any_preds && !any_succs) {
            CFGEdge EDummy = make_pair(nullptr, &BB);
            auto DFV = ELA.at(EDummy);
            sstr_tmp << "  Dummy edges:\n";
            sstr_tmp << "    " << to_string(&EDummy) << "\n";
            sstr_tmp << MAG << "      " << to_string(DFV) << "\n" << BLK;
            sstr << str_tmp;
        }
        str_tmp = "";
        sstr_tmp << "-----\n\n";
    }
    sstr << str_tmp;
    str_tmp = "";
}


void print_block_level_analysis(const BlockLevelAnalysis& BLA, const Function& F, raw_string_ostream& sstr) {
    for (auto& BB : F) {
        const DataFlowValue& DFV = BLA.at(&BB);
        bool any_decl = !DFV.empty();
        if (any_decl) {
            sstr <<  BLK;
            sstr << to_string(&BB) << "\n";
            sstr << "-----\n";
            sstr << MAG << "  " << to_string(DFV) << "\n" << BLK;
            sstr << "-----\n\n";
        }
    }
}


CFGEdgeSet get_input_edges(const BasicBlock& BB) {
    CFGEdgeSet edges;
    for (auto* BBPrev : predecessors(&BB)) {
        CFGEdge EIn = make_pair(BBPrev, &BB);
        edges.insert(EIn);
    }
    if (edges.empty()) {
        CFGEdge EDummy = make_pair(nullptr, &BB);
        edges.insert(EDummy);
    }
    return edges;
}


CFGEdgeSet get_output_edges(const BasicBlock& BB) {
    CFGEdgeSet edges;
    for (auto* BBNext : successors(&BB)) {
        CFGEdge EOut = make_pair(&BB, BBNext);
        edges.insert(EOut);
    }
    if (edges.empty()) {
        CFGEdge EDummy = make_pair(&BB, nullptr);
        edges.insert(EDummy);
    }
    return edges;
}


CFGEdgeSet get_edges(const BasicBlock& BB) {
    CFGEdgeSet edges = get_input_edges(BB);
    auto output_edges = get_output_edges(BB);
    edges.insert(output_edges.begin(), output_edges.end());
    return edges;
}


CFGEdgeSet get_all_edges(const Function& F) {
    CFGEdgeSet all_edges;
    for (auto& BB : F) {
        auto edges = get_edges(BB);
        for (auto edge : edges) {
            all_edges.insert(edge);
        }
    }
    return all_edges;
}


YAML::Emitter& operator<< (YAML::Emitter& yout, const CFGEdge& E) {
    yout << YAML::Flow
	     << YAML::BeginSeq
            << YAML::DoubleQuoted << to_string(E.first)
            << YAML::DoubleQuoted << to_string(E.second)
         << YAML::EndSeq;
	return yout;
}


// TODO: Order all the values in a data-flow value
YAML::Emitter& operator<< (YAML::Emitter& yout, const DataFlowValue& DFV) {
    yout << YAML::Flow
	     << YAML::BeginSeq;
    for (auto V : DFV) {
        yout << YAML::DoubleQuoted << to_string(V);
    }
    yout << YAML::EndSeq;
	return yout;
}


// TODO: Order the edges in the ELA
YAML::Emitter& operator<< (YAML::Emitter& yout, const EdgeLevelAnalysis& ELA) {
    yout << YAML::BeginMap;
    for (const auto& [E, DFV] : ELA) {
        yout << YAML::Key << E
             << YAML::Value << DFV;
    }
    yout << YAML::EndMap;
    return yout;
}


// TODO: Order the blocks in the BLA
YAML::Emitter& operator<< (YAML::Emitter& yout, const BlockLevelAnalysis& BLA) {
    yout << YAML::BeginMap;
    for (const auto& [BB, DFV] : BLA) {
        yout << YAML::Key << YAML::DoubleQuoted << to_string(BB)
             << YAML::Value << DFV;
    }
    yout << YAML::EndMap;
    return yout;
}


// TODO: Order the blocks in the knowledge frontier
YAML::Emitter& operator<< (YAML::Emitter& yout, const KnowledgeFrontierMap& frontier) {
    yout << YAML::BeginMap;
    for (const auto& [v, bb_ary] : frontier) {
        yout << YAML::Key << YAML::DoubleQuoted << to_string(v);
        yout << YAML::Flow
	         << YAML::BeginSeq;
        for(auto BB : bb_ary)
            yout << YAML::DoubleQuoted  << to_string(BB);
        yout << YAML::EndSeq;
    }
    yout << YAML::EndMap;
    return yout;
}


void serialize_ELA(YAML::Emitter& yout, const string phase_name,
                   const EdgeLevelAnalysis& ELA, const int& pass_counter) {
    yout << YAML::BeginMap
            << YAML::Key << "phase_name"
            << YAML::Value << phase_name
            << YAML::Key << "pass_count"
            << YAML::Value << pass_counter
            << YAML::Key << "type"
            << YAML::Value << "edge-level"
            << YAML::Key << "state"
            << YAML::Value << ELA
         << YAML::EndMap;
}


void serialize_BLA(YAML::Emitter& yout, const string phase_name,
                   const BlockLevelAnalysis& BLA, const int& pass_counter) {
    yout << YAML::BeginMap
            << YAML::Key << "phase_name"
            << YAML::Value << phase_name
            << YAML::Key << "pass_count"
            << YAML::Value << pass_counter
            << YAML::Key << "type"
            << YAML::Value << "block-level"
            << YAML::Key << "state"
            << YAML::Value << BLA
         << YAML::EndMap;
}

void serialize_knowledge_frontier(YAML::Emitter& yout, const string phase_name,
                                  const KnowledgeFrontierMap& frontier, const int& pass_counter) {
    yout << YAML::BeginMap
            << YAML::Key << "phase_name"
            << YAML::Value << phase_name
            << YAML::Key << "pass_count"
            << YAML::Value << pass_counter
            << YAML::Key << "state"
            << YAML::Value << frontier
         << YAML::EndMap;
}

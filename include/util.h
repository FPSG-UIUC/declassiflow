#ifndef UTIL_H
#define UTIL_H

#include "llvm/IR/IRBuilder.h"

#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <set>
#include <string>
#include <sstream>
#include <algorithm>

#include "yaml-cpp/yaml.h"

// TODO: Provide a way to order two edges
typedef std::pair<const llvm::BasicBlock*, const llvm::BasicBlock*> CFGEdge;

struct CFGEdgeHash {
    std::size_t operator () (const CFGEdge& p) const {
        auto h1 = std::hash<const llvm::BasicBlock*>{}(p.first);
        auto h2 = std::hash<const llvm::BasicBlock*>{}(p.second);
        // https://stackoverflow.com/a/27952689/1227353
        h1 ^= h2 + 0x9e3779b9 + (h1 << 6) + (h1 >> 2);
        return h1;
    }
};

typedef std::unordered_set<CFGEdge, CFGEdgeHash> CFGEdgeSet;

typedef std::unordered_set<const llvm::Value*> ValueSet;
typedef std::unordered_set<const llvm::BasicBlock*> BlockSet;
typedef std::unordered_set<const llvm::Value*> DataFlowValue; // TODO: Replace usages of this with ValueSet
typedef std::unordered_map<CFGEdge, DataFlowValue, CFGEdgeHash> EdgeLevelAnalysis;
typedef std::unordered_map<const llvm::BasicBlock*, DataFlowValue> BlockLevelAnalysis;
typedef std::unordered_map<std::string, std::vector<std::string>> PseudoBlockLevelAnalysis;
typedef std::map<llvm::Instruction*, std::vector<llvm::User*>> DefUseMap;
typedef std::map<const llvm::Value*, ValueSet> ValueTreeMap;
typedef std::unordered_map<llvm::BasicBlock*, llvm::BasicBlock*> MergeNodeMap;
typedef std::unordered_map<const llvm::Value*, BlockSet> KnowledgeFrontierMap;
typedef std::vector<const llvm::Instruction*> InstList;

PseudoBlockLevelAnalysis pseudo_BLA_from_YAML(llvm::Function& F, YAML::Node& s);
bool append_YAML_to_ELA(llvm::Function& F, YAML::Node& s, EdgeLevelAnalysis& ELA);

bool is_access(const llvm::Instruction* I);
bool is_transmit(const llvm::Instruction* I);
bool is_dataflow_instr(const llvm::Instruction* I);
bool is_const_val(const llvm::Value* V);

const llvm::Value* leaked_op(const llvm::Instruction& I);

bool is_user_function(const llvm::Function& F);

std::string to_string(const llvm::Value* V);
std::string to_string(const llvm::Module* M);
std::string to_string(const CFGEdge* E);
std::string to_string(const DataFlowValue& DFV);

// TODO: Add a subset (and maybe strict subset) method if it can simplify the code elsewhere
// TODO: Add a union method
// TODO: Make these functions templated so they can be used with other types
// TODO: Overloaded operators?
void intersection(const DataFlowValue& A, const DataFlowValue& B, DataFlowValue& result);
void difference(const DataFlowValue& A, const DataFlowValue& B, DataFlowValue& result);

void print_blocks_and_edges(const llvm::Function& F, llvm::raw_string_ostream& sstr);
void print_edge_level_analysis(const EdgeLevelAnalysis& ELA, const llvm::Function& F, llvm::raw_string_ostream& sstr);
void print_block_level_analysis(const BlockLevelAnalysis& BLA, const llvm::Function& F, llvm::raw_string_ostream& sstr);

CFGEdgeSet get_all_edges(const llvm::Function& F);
CFGEdgeSet get_edges(const llvm::BasicBlock& BB);
CFGEdgeSet get_input_edges(const llvm::BasicBlock& BB);
CFGEdgeSet get_output_edges(const llvm::BasicBlock& BB);

ValueSet value_tree_children_of(ValueTreeMap& value_tree, const llvm::Value* parent);

YAML::Emitter& operator << (YAML::Emitter& yout, const CFGEdge& E);
YAML::Emitter& operator << (YAML::Emitter& yout, const DataFlowValue& DFV);
YAML::Emitter& operator << (YAML::Emitter& yout, const EdgeLevelAnalysis& ELA);
YAML::Emitter& operator << (YAML::Emitter& yout, const BlockLevelAnalysis& BLA);

void serialize_ELA(YAML::Emitter& yout, const std::string pass_name,
                   const EdgeLevelAnalysis& ELA, const int& pass_counter);
void serialize_BLA(YAML::Emitter& yout, const std::string pass_name,
                   const BlockLevelAnalysis& BLA, const int& pass_counter);
void serialize_knowledge_frontier(YAML::Emitter& yout, const std::string phase_name,
                                  const KnowledgeFrontierMap& frontier, const int& pass_counter);

#endif

#ifndef CLEANUP_H
#define CLEANUP_H

#include "llvm/IR/IRBuilder.h"

#include "util.h"

void reduce(const EdgeLevelAnalysis& ELA, const llvm::Function& F,
            BlockLevelAnalysis& BLA);

void loop_map(BlockLevelAnalysis& BLA, const llvm::Function& F,
              ValueTreeMap& value_tree,
              ValueSet& original_values);

void loop_reduce(BlockLevelAnalysis& BLA, const llvm::Function& F,
                 ValueSet& original_values);


// TODO: this should populate a KnowledgeFrontierMap given as an argument; it
// should NOT return a value. This is so that its interface is consistent with
// that of all the other functions
KnowledgeFrontierMap knowledge_frontier(llvm::Function& F,
                                        const BlockLevelAnalysis& BLA);

#endif

#ifndef PASSES_H
#define PASSES_H

#include "llvm/IR/IRBuilder.h"
#include "util.h"

bool find_transmits(EdgeLevelAnalysis& ELA, const llvm::Function& F, ValueSet& transmitted_vals,
                    std::unordered_map<const llvm::Value*, BlockSet>& transmitter_locs);
bool intra_edge(EdgeLevelAnalysis& ELA, const llvm::Function& F);
bool inter_edge(EdgeLevelAnalysis& ELA, const llvm::Function& F);
bool phi_prop(EdgeLevelAnalysis& ELA, const llvm::Function& F);
void knowledge_expansion(EdgeLevelAnalysis& ELA, const llvm::Function& F);

#endif


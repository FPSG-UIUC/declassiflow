#ifndef FORWARD_H
#define FORWARD_H

#include "llvm/IR/IRBuilder.h"
#include "util.h"

bool fwd_intra(EdgeLevelAnalysis& ELA, const llvm::Function& F);
bool fwd_inter(EdgeLevelAnalysis& ELA, const llvm::Function& F);

#endif

#ifndef BACKWARD_H
#define BACKWARD_H

#include "llvm/IR/IRBuilder.h"
#include "util.h"

bool bwd_intra(EdgeLevelAnalysis& ELA, const llvm::Function& F);
bool bwd_inter(EdgeLevelAnalysis& ELA, const llvm::Function& F);

#endif

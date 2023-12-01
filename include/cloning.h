#ifndef DECLASSIFLOW_CLONING_H
#define DECLASSIFLOW_CLONING_H

#include "llvm/IR/Dominators.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Utils/ValueMapper.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Analysis/LoopInfo.h"
#include "util.h"

void expand_loop(llvm::Function& F, llvm::Loop* loop, DefUseMap& DUM, ValueTreeMap& value_tree);

ValueSet get_original_values(llvm::Function& F);

#endif

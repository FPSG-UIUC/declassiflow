#ifndef DECLASSIFLOW_LOOP_H
#define DECLASSIFLOW_LOOP_H

#include "llvm/Analysis/LoopInfo.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/ADT/APFloat.h"
#include "util.h"
#include "cloning.h"

bool loop_condition_check(llvm::Function& F);
llvm::LoopInfo get_all_loops(llvm::Function& F);
DefUseMap get_outgoing_values(llvm::Function& F, llvm::LoopInfo& LI);
ValueTreeMap expand_all_loops(llvm::Function& F, DefUseMap& DUM);

#endif

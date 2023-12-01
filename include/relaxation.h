// DESCRIPTION:
//    Declares the Relaxation pass
//
//==============================================================================
#ifndef RELAXATION_PASS_H
#define RELAXATION_PASS_H

#include "llvm/IR/PassManager.h"
#include "llvm/Pass.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"

#include "util.h"

//------------------------------------------------------------------------------
// New PM interface
//------------------------------------------------------------------------------
struct Relaxation : public llvm::PassInfoMixin<Relaxation> {
    llvm::PreservedAnalyses run(llvm::Module &M,
                                llvm::ModuleAnalysisManager &);
};
#endif
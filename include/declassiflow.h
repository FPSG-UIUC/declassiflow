// DESCRIPTION:
//    Declares the Declassiflow pass
//
//==============================================================================
#ifndef DECLASSIFLOW_PASS_H
#define DECLASSIFLOW_PASS_H

#include "llvm/IR/PassManager.h"
#include "llvm/Pass.h"
#include "util.h"

// TODO: This should really be in it's own header/source file.
class InterFunctionLeakage {
    public:
        InterFunctionLeakage(llvm::Module* M);
        // add arguments that are leaked for a function
        void add_arg_leakage(const llvm::Function* F, const llvm::Value* leaked);
        // fetch arguments leaked by function (I must be the call instruction!)
        ValueSet get_arg_leakage(const llvm::Function* F,
                                                         const llvm::CallInst* I);

    private:
        llvm::Module* M;
        std::unordered_map<const llvm::Function*, std::vector<int>> function_to_arg_leakage;
};


//------------------------------------------------------------------------------
// New PM interface
//------------------------------------------------------------------------------
struct Declassiflow : public llvm::PassInfoMixin<Declassiflow> {
    llvm::PreservedAnalyses run(llvm::Module &M,
                                llvm::ModuleAnalysisManager &);
};

#endif

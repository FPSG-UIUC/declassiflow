// DESCRIPTION:
//    Declares the Flagging pass
//
//==============================================================================
#ifndef FLAGGING_PASS_H
#define FLAGGING_PASS_H

#include "llvm/IR/PassManager.h"
#include "llvm/Pass.h"
#include "util.h"

#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/Module.h"
#include "llvm/ADT/APInt.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/Transforms/Utils/Cloning.h"

#include <unordered_map>
#include <vector>
#include <set>
#include <tuple>
#include <string>
#include <sstream>
#include <algorithm>
#include <queue>
#include <iostream>
#include <fstream>
#include <filesystem>

#define SINGLE_ASSERT

// TODO: This should really be in it's own header/source file.
class VariableFlagMap {
    public:
        VariableFlagMap();                                                      // constructor
        VariableFlagMap(llvm::Function& F);                                     // constructor
        size_t size();                                                          // returns size (# of transmitters)
        void print();                                                           // human readable output
        llvm::BasicBlock* get_transmitter_block(size_t i);                      // get transmitter for i
        std::vector<llvm::BasicBlock*> get_all_transmitter_blocks();            // get transmitter for i
        std::vector<llvm::Value*> get_flags(size_t i);                          // returns a vector of the flags for transmitter i
        ValueSet get_unique_flags();                                            // returns a unordered set of the unique flags
        std::vector<llvm::Value*> get_flag_array();                             // get the flag array
        int get_flag_index(llvm::Value* flag);                                  // given flag get the index of that flag
        bool is_empty();                                                        // is the VFM empty

    private:
        void add_flag(int transmitter, llvm::Value* flag);                      // adds a flag ptr to transmitter
        size_t num_mappings;                                                    // size (in other words, # of transmitters)
        std::vector<llvm::BasicBlock*> transmitter_blocks;                      // maps index i -> transmitter block
        std::vector<std::vector<llvm::Value*>> transmit_to_flag_map;            // transmitters -> flag ptr mapping
        ValueSet unique_flags;                                                  // stores unique flag ptrs
        std::vector<llvm::Value*> flag_array;                                   // stores the array of flags to get the indexes for each flag

        // Can very easily add flags by using memory objects!!!
        // source: https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/LangImpl07.html
};

void instrument(llvm::Function& F, VariableFlagMap& VFM);

//------------------------------------------------------------------------------
// New PM interface
//------------------------------------------------------------------------------
struct Flagging : public llvm::PassInfoMixin<Flagging> {
    llvm::PreservedAnalyses run(llvm::Function &F,
                                llvm::FunctionAnalysisManager &);
};
#endif
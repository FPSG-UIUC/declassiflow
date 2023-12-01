#include "flagging.h"

using namespace llvm;
using namespace std;
namespace fs = std::filesystem;

// TODO: Everything printed to errs() should be printed to outs()
// but only if we're in debug mode, and everything originally printed
// to outs() should be directly written to a YAML file
#define DEBUG_TYPE "flagging"

// Defined in declassiflow.cpp
extern cl::opt<string> InputFilename;

cl::opt<string> OutputFolder("output",
                             cl::desc("Folder where results will be written"),
                             cl::value_desc("folder"));

void instrument(Function &F,
                VariableFlagMap &VFM,
                ValueToValueMapTy& vmap,
                BasicBlock* target_block) {
    Module *M = F.getParent();

    // Set up IR builder
    LLVMContext &ctx = F.getContext();
    IRBuilder<> builder(ctx);
    builder.SetInsertPoint(&(F.getEntryBlock().front()));

    // Create global variables
    M->getOrInsertGlobal("declassiflow.flag.pos.const", builder.getInt8Ty());
    M->getOrInsertGlobal("declassiflow.flag.neg.const", builder.getInt8Ty());
    M->getOrInsertGlobal("declassiflow.flag.null.const", builder.getInt8Ty());
    M->getOrInsertGlobal("declassiflow.flag.num", builder.getInt64Ty());
    GlobalVariable* assert_string = builder.CreateGlobalString("DECLASSIFLOW ASSERT FAILED",
                                                               "declassiflow.assert.string");
    GlobalVariable* flag_pos_const = M->getNamedGlobal("declassiflow.flag.pos.const");
    GlobalVariable* flag_neg_const = M->getNamedGlobal("declassiflow.flag.neg.const");
    GlobalVariable* flag_null_const = M->getNamedGlobal("declassiflow.flag.null.const");
    GlobalVariable* flag_num = M->getNamedGlobal("declassiflow.flag.num");
    ConstantInt* pos_val_literal = ConstantInt::get(M->getContext(), APInt(8, 1));
    ConstantInt* neg_val_literal = ConstantInt::get(M->getContext(), APInt(8, -1));
    ConstantInt* null_vall_literal = ConstantInt::get(M->getContext(), APInt(8, 0));
    ConstantInt *init_flag_num = ConstantInt::get(M->getContext(), APInt(64, -1));
    flag_pos_const->setInitializer(pos_val_literal);
    flag_neg_const->setInitializer(neg_val_literal);
    flag_null_const->setInitializer(null_vall_literal);
    flag_num->setInitializer(init_flag_num);

    // Set up KLEE functions (__assert_fail and klee_make_symbolic)
    vector<Type*> arg_types = {Type::getInt8PtrTy(M->getContext()),
                               Type::getInt8PtrTy(M->getContext()),
                               Type::getInt32Ty(M->getContext()),
                               Type::getInt8PtrTy(M->getContext())};
    FunctionType* FT = FunctionType::get(Type::getVoidTy(M->getContext()), arg_types, false);
    Function* assert_fail_function = F.getParent()->getFunction("__assert_fail");
    if(!assert_fail_function) {
        assert_fail_function = Function::Create(FT, Function::ExternalLinkage, "__assert_fail", M);
        assert_fail_function->setDSOLocal(true);
    }
    arg_types = {Type::getInt8PtrTy(M->getContext()),
                 Type::getInt64Ty(M->getContext()),
                 Type::getInt8PtrTy(M->getContext())};
    FT = FunctionType::get(Type::getVoidTy(M->getContext()), arg_types, false);
    Function* klee_make_symbolic = F.getParent()->getFunction("klee_make_symbolic");
    if(klee_make_symbolic == nullptr) {
        klee_make_symbolic = Function::Create(FT, Function::ExternalLinkage, "klee_make_symbolic", M);
        klee_make_symbolic->setDSOLocal(true);
    }

    // Set up basic ID info for asserts
    GlobalVariable* file_name = builder.CreateGlobalString(M->getSourceFileName(), "str.file");
    GlobalVariable* func_name = builder.CreateGlobalString(F.getName(), "str.func");

    // Load global constants
    LoadInst* pos_const_load  = builder.CreateLoad(Type::getInt8Ty(F.getContext()),
                                                   dyn_cast<Value>(flag_pos_const),
                                                   "flag.pos.const");
    LoadInst* neg_const_load  = builder.CreateLoad(Type::getInt8Ty(F.getContext()),
                                                   dyn_cast<Value>(flag_neg_const),
                                                   "flag.neg.const");
    LoadInst* null_const_load = builder.CreateLoad(Type::getInt8Ty(F.getContext()),
                                                   dyn_cast<Value>(flag_null_const),
                                                   "flag.null.const");

    // Create buffer to hold values for all flags
    auto unique_flags = VFM.get_unique_flags();
    auto flag_array = VFM.get_flag_array();
    assert(!unique_flags.empty());
    assert(flag_array.size() == unique_flags.size());
    ArrayType *flag_array_type = ArrayType::get(Type::getInt8Ty(F.getContext()), unique_flags.size());
    AllocaInst* flag_buffer = builder.CreateAlloca(flag_array_type, nullptr, "declassiflow.flag.buffer");

    // Initialize all flags to null in entry block
    ConstantInt* zero_const = ConstantInt::get(M->getContext(), APInt(64, 0, true));
    for(int i = 0; i < flag_array.size(); i++) {
        ConstantInt* index_const = ConstantInt::get(M->getContext(), APInt(64, i, true));
        Value* flag_GEP = builder.CreateInBoundsGEP(flag_array_type,
                                                    flag_buffer,
                                                    {zero_const, index_const},
                                                    "declassiflow.flag.ptr");
        builder.CreateStore(null_const_load, flag_GEP);
    }

    // Set flags to negative in target block
    target_block = dyn_cast<BasicBlock>(vmap[target_block]);
    builder.SetInsertPoint(target_block->getTerminator());
    for(int i = 0; i < flag_array.size(); i++) {
        ConstantInt* index_const = ConstantInt::get(M->getContext(), APInt(64, i, true));
        Value* flag_GEP = builder.CreateInBoundsGEP(flag_array_type,
                                                    flag_buffer,
                                                    {zero_const, index_const},
                                                    "declassiflow.flag.ptr");
        builder.CreateStore(neg_const_load, flag_GEP);
    }

    // Set flags to postive in transmitter blocks
    for (size_t i = 0; i < VFM.size(); i++) {
        BasicBlock* transmitter_block = dyn_cast<BasicBlock>(vmap[VFM.get_transmitter_block(i)]);
        assert(transmitter_block != nullptr);
        auto flags = VFM.get_flags(i);
        // builder.SetInsertPoint(transmitter_block, transmitter_block->getFirstInsertionPt());
        builder.SetInsertPoint(transmitter_block->getTerminator());
        for (auto f : flags) {
            int flag_idx = VFM.get_flag_index(f);
            assert(flag_idx != -1);
            ConstantInt* flag_index_const = ConstantInt::get(M->getContext(), APInt(64, flag_idx, true));
            Value* flag_GEP = builder.CreateInBoundsGEP(flag_array_type,
                                                        flag_buffer,
                                                        {zero_const, flag_index_const},
                                                        "declassiflow.flag.ptr");
            builder.CreateStore(pos_const_load, flag_GEP);
        }
    }

    // Upon success of KLEE assert, return a default value
    // NOTE: since we're not testing correctness and we're limiting our scope
    //  to one function at a time, we don't care that the return value isn't correct
    // TODO: make this more automatic. There must be some way to construct
    //  a "default" value of whatever the return type is
    BasicBlock* klee_assert_success = BasicBlock::Create(F.getContext(),
                                                         "klee.assert.success",
                                                         &F,
                                                         nullptr);
                                                         vector<BasicBlock*> returning_blocks;
    Type* ret_type = F.getReturnType();
    builder.SetInsertPoint(klee_assert_success);
    if(ret_type->isVoidTy()) {
        builder.CreateRetVoid();
    }
    else if(ret_type->isIntegerTy()) {
        ConstantInt* ret_const = ConstantInt::get(M->getContext(),
                                                  APInt(ret_type->getScalarSizeInBits(), 0));
        builder.CreateRet(ret_const);
    }
    else {
        errs() << "Return type" << *ret_type << " unsupported for now!\n";
        exit(1);
    }

    // Create block for assertion failure
    BasicBlock* klee_assert_failed = BasicBlock::Create(F.getContext(),
                                                        "klee.assert.fail",
                                                        &F,
                                                        nullptr);
    ConstantInt* line_const = ConstantInt::get(M->getContext(), APInt(32, 0));
    auto func_name_type = func_name->getType()->getContainedType(0);
    auto file_name_type = file_name->getType()->getContainedType(0);
    auto assert_str_type = assert_string->getType()->getContainedType(0);
    builder.SetInsertPoint(klee_assert_failed);
    Value* func_GEP = builder.CreateInBoundsGEP(func_name_type,
                                                func_name,
                                                {zero_const, zero_const});
    Value* file_GEP = builder.CreateInBoundsGEP(file_name_type,
                                                file_name,
                                                {zero_const, zero_const});
    Value* assert_GEP = builder.CreateInBoundsGEP(assert_str_type,
                                                  assert_string,
                                                  {zero_const, zero_const});
    builder.CreateCall(assert_fail_function, {assert_GEP, file_GEP, line_const, func_GEP});
    builder.CreateUnreachable();

    // All previous returning blocks now go to the KLEE assertion
    BasicBlock* klee_assert_check = BasicBlock::Create(F.getContext(),
                                                      "klee.assert.check",
                                                      &F,
                                                      nullptr);
    for (auto& BB : F) {
        if (BB.getTerminator() == nullptr) continue;
        if (!isa<ReturnInst>(BB.getTerminator())) continue;
        if (&BB == klee_assert_success) continue;
        BB.getTerminator()->eraseFromParent(); // Remove the return inst
        builder.SetInsertPoint(&BB);
        builder.CreateBr(klee_assert_check);
    }
    builder.SetInsertPoint(klee_assert_check);
    LoadInst* flag_index = builder.CreateLoad(Type::getInt64Ty(F.getContext()),
                                              dyn_cast<Value>(flag_num),
                                              "declassiflow.flag.index");
    Value* flag_GEP = builder.CreateInBoundsGEP(flag_array_type,
                                                flag_buffer,
                                                {zero_const, flag_index},
                                                "declassiflow.flag.ptr");
    LoadInst* flag_value = builder.CreateLoad(Type::getInt8Ty(F.getContext()),
                                              flag_GEP,
                                              "declassiflow.flag.value");
    Value* cmp_inst = builder.CreateICmpNE(flag_value, neg_const_load, "declassiflow.check.pred");
    builder.CreateCondBr(cmp_inst, klee_assert_success, klee_assert_failed);
}


PreservedAnalyses Flagging::run(Function &F,
                                FunctionAnalysisManager &) {
    if (InputFilename.empty()) {
        errs() << "ERROR: YAML input file is required!\n";
        exit(1);
    }

    string input_file_name = InputFilename;
    fs::path input_file_path(input_file_name);
    if (!fs::exists(input_file_path)) {
        errs() << "ERROR: Provided input file doesn't exist!\n";
        exit(1);
    }

    if (OutputFolder == "") {
        errs() << "ERROR: An output folder MUST be provided!\n";
        exit(1);
    }

    string output_folder_name = OutputFolder;
    fs::path output_folder_path(output_folder_name);
    output_folder_path = output_folder_path / to_string(F.getParent()) / to_string(&F);
    if (!fs::exists(output_folder_path)) {
        errs() << "Provided output folder doesn't exist, creating it\n";
        fs::create_directories(output_folder_path);
    }

    errs() << "Running flagging pass on function " << F.getName() << " \n";

    // Create datastructure
    VariableFlagMap VFM = VariableFlagMap(F);
    if(VFM.is_empty()) {
        errs() << "  no flags, skipping...\n";
        errs() << "-------\n";
        return PreservedAnalyses::none();
    }
    VFM.print();

    DominatorTree dom_tree(F);

    vector<BasicBlock*> transmitter_blocks = VFM.get_all_transmitter_blocks();

    // Find all blocks that dominate all transmitter-containing blocks
    errs() << "Target blocks:\n";
    vector<BasicBlock*> target_blocks;
    for(BasicBlock& BB : F) {
        bool valid = true;
        for(BasicBlock* transmit_block : transmitter_blocks) {
            if(dom_tree.dominates(&BB, transmit_block) == false) {
                valid = false;
                break;
            }
        }
        if(valid) {
            errs() << "  " << to_string(&BB) << "\n";
            target_blocks.push_back(&BB);
        }
    }

    // Write target block names to file
    ofstream klee_targets_file(output_folder_path/"targets.txt");
    for(auto target_block : target_blocks) {
        klee_targets_file << to_string(target_block) << "\n";
    }
    klee_targets_file.close();

    // Write flag names to file (line number matches their index)
    ofstream klee_flags_file(output_folder_path/"flags.txt");
    vector<Value*> flag_array = VFM.get_flag_array();
    for (int i = 0; i < flag_array.size(); i++) {
        klee_flags_file << to_string(flag_array[i]) << "\n";
    }
    klee_flags_file.close();

    // Write function name to a file
    ofstream func_name_file(output_folder_path/"func_name.txt");
    func_name_file << F.getName().str() << "\n";
    func_name_file.close();

    // Write a hidden, empty, dummy file so that later we can check
    // that a helper script is running in the right folder
    ofstream confirmation_file(output_folder_path/"../.confirmation");
    confirmation_file << "";
    confirmation_file.close();

    // Produce instrumented files, one for each target block
    int klee_id = 0;
    for(BasicBlock* target_block : target_blocks) {
        ValueToValueMapTy vmap;
        Function* F_clone = CloneFunction(&F, vmap);

        F_clone->setName(F.getName() + "_KLEE");

        instrument(*F_clone, VFM, vmap, target_block);

        // Write instrumented code to file
        string filename = "check_target_" + to_string(klee_id++) + ".ll";
        ofstream instrumented_file(output_folder_path/filename);
        string instrumented_code;
        raw_string_ostream sstr(instrumented_code);
        sstr << *(F.getParent());
        sstr.flush();
        instrumented_file << instrumented_code;
        instrumented_file.close();

        // Remove cloned function from module
        F_clone->eraseFromParent();

        // Remove globals created
        Module* M = F.getParent();
        GlobalVariable* str_func = M->getNamedGlobal("str.func");
        str_func->eraseFromParent();
        GlobalVariable* str_file = M->getNamedGlobal("str.file");
        str_file->eraseFromParent();
        GlobalVariable* str_assert = M->getNamedGlobal("declassiflow.assert.string");
        str_assert->eraseFromParent();
    }

    errs() << "-------\n";

    return PreservedAnalyses::none();
}

// Class implementation
VariableFlagMap::VariableFlagMap(Function &F) {
    // grab data
    PseudoBlockLevelAnalysis PseudoBLA;
    try {
        YAML::Node config = YAML::LoadFile(InputFilename);
        PseudoBLA = pseudo_BLA_from_YAML(F, config);
    }
    catch(...) { // TODO: forward the error message from yaml-cpp (YAML::ParserException)
        errs() << "ERROR: Could not open provded YAML file!\n";
        exit(1);
    }

    // constructor
    num_mappings = 0;

    // Get transmitter blocks
    BlockSet block_check_set;
    unordered_map<string, Value*> names_to_values;
    for (auto &BB : F) {
        for (auto &I : BB) {
            // map all string names to their value pointer
            if(to_string((Value *)&I) == "???") {
                errs() << "FOUND A ???\n";
                exit(1);
            }
            names_to_values[to_string((Value *)&I)] = (Value*)&I;

            // check if block contains a transmitter
            if (is_transmit(&I)) {
                if(block_check_set.contains(&BB) == false) {
                    num_mappings++;
                    transmitter_blocks.push_back(&BB);
                    block_check_set.insert(&BB);
                }
            }
        }
    }

    for (auto& Arg : F.args()) {
        names_to_values[to_string((Value *)&Arg)] = (Value*)&Arg;
    }

    transmit_to_flag_map = vector(num_mappings, vector<Value *>(0, nullptr));
    for (int i = 0; i < num_mappings; i++) {
        BasicBlock* BB = transmitter_blocks[i];
        auto possible_flags = PseudoBLA[to_string(BB)];
        for (string& f : possible_flags) {
            add_flag(i, names_to_values[f]);
        }
    }
}

size_t VariableFlagMap::size() {
    return num_mappings;
}

void VariableFlagMap::print() {
    for (size_t i = 0; i < num_mappings; i++) {
        errs() << "Transmitter block: " << to_string(transmitter_blocks[i]) << "\n";
        errs() << "  Flags: ";
        for (Value *v : transmit_to_flag_map[i])
            errs() << to_string(v) << " ";
        errs() << "\n";
    }
    errs() << "  Unique flags: ";
    for (auto v : unique_flags)
        errs() << to_string(v) << " ";
    errs() << "\n";
}

BasicBlock* VariableFlagMap::get_transmitter_block(size_t i) {
    return transmitter_blocks[i];
}

void VariableFlagMap::add_flag(int transmitter, Value *flag) {
    // make sure its unique
    for (Value *v : transmit_to_flag_map[transmitter])
        if (v == flag)
            return;

    // add to transmitter
    transmit_to_flag_map[transmitter].push_back(flag);

    // maintain list of unique flags
    if(unique_flags.contains(flag) == false)
        flag_array.push_back(flag);
    unique_flags.insert(flag);
}

ValueSet VariableFlagMap::get_unique_flags() {
    return unique_flags;
}

vector<Value*> VariableFlagMap::get_flags(size_t i) {
    return transmit_to_flag_map[i];
}

vector<Value*> VariableFlagMap::get_flag_array() {
    return flag_array;
}

int VariableFlagMap::get_flag_index(Value* flag) {
    for(int i = 0; i < flag_array.size(); i++) {
        if(flag_array[i] == flag) return i;
    }
    return -1; // not found
}

vector<BasicBlock*> VariableFlagMap::get_all_transmitter_blocks() {
    return transmitter_blocks;
}

bool VariableFlagMap::is_empty() {
    return unique_flags.empty();
}
#include "passes.h"

#include "llvm/IR/InstIterator.h"

using namespace std;
using namespace llvm;

bool fwd_transfer(DataFlowValue& DFV, const Instruction& I) {
    if (DFV.contains(&I)) return false;

    if (auto Select = dyn_cast<SelectInst>(&I)) {
        auto VTrue = Select->getTrueValue();
        auto VFalse = Select->getFalseValue();
        bool true_val_decl = is_const_val(VTrue) || DFV.contains(VTrue);
        bool false_val_decl = is_const_val(VFalse) || DFV.contains(VFalse);
        if (true_val_decl && false_val_decl && !DFV.contains(Select)) {
            DFV.insert(Select);
            return true;
        }
        else {
            return false;
        }
    }
    else {
        for (auto& U : I.operands()) {
            Value* V = U.get();
            if (is_const_val(V)) continue;
            if (!DFV.contains(V)) return false;
        }
        DFV.insert(&I);
        return true;
    }
}

bool bwd_transfer(DataFlowValue& DFV, const Instruction& I) {
    if (isa<CastInst>(I)) {
        auto* Op = I.getOperand(0);
        if (DFV.contains(&I) && !DFV.contains(Op)) {
            DFV.insert(Op);
            return true;
        }
    }
    else if (isa<BinaryOperator>(I)) {
        auto opcode = I.getOpcode();
        if (opcode == Instruction::Add || opcode == Instruction::Sub) {
            Value* op1 = I.getOperand(0);
            Value* op2 = I.getOperand(1);
            bool op1_decl = DFV.contains(op1) || is_const_val(op1);
            bool op2_decl = DFV.contains(op2) || is_const_val(op2);
            bool res_decl = DFV.contains(&I);
            if (res_decl && (op1_decl != op2_decl)) {
                Value* OpToDecl = nullptr;
                if (!op1_decl) {
                    OpToDecl = op1;
                }
                else if (!op2_decl) {
                    OpToDecl = op2;
                }
                DFV.insert(OpToDecl);
                return true;
            }
        }
    }
    else if (auto GEP = dyn_cast<GetElementPtrInst>(&I)) {
        if (!DFV.contains(GEP)) return false;
        Value* OpToDecl = nullptr;
        Value* VPtr = (Value*)GEP->getPointerOperand();
        int num_not_decl = 0;
        if (!DFV.contains(VPtr) && !is_const_val(VPtr)) {
            OpToDecl = VPtr;
            num_not_decl++;
        }
        for (const Use& U : GEP->indices()) {
            Value* VIdx = U.get();
            if (!DFV.contains(VIdx) && !is_const_val(VIdx)) {
                if (num_not_decl == 0) {
                    OpToDecl = VIdx;
                    num_not_decl++;
                }
                else {
                    return false;
                }
            }
        }
        if (OpToDecl != nullptr) {
            DFV.insert(OpToDecl);
            return true;
        }
    }

    return false;
}


bool intra_edge(EdgeLevelAnalysis& ELA, const Function& F) {
    bool pass_did_work = false;

    for (auto& BB : F) {
        for (auto it = inst_begin(F); it != inst_end(F); ++it) {
            auto& I = *it;
            if (!is_dataflow_instr(&I)) continue;
            for (auto E : get_edges(BB)) {
                DataFlowValue& DFV = ELA[E];
                pass_did_work |= fwd_transfer(DFV, I);
                pass_did_work |= bwd_transfer(DFV, I);
            }
        }
    }

    return pass_did_work;
}


bool fwd_inter(EdgeLevelAnalysis& ELA, const Function& F)
{
    bool pass_did_work = false;

    for (auto& BB : F) {
        DataFlowValue DFVInInxn;
        bool first_set = true;
        for (auto EIn : get_input_edges(BB)) {
            DataFlowValue& DFVIn = ELA[EIn];
            DataFlowValue DFVTmp;
            if (first_set) {
                DFVTmp = DFVIn;
                first_set = false;
            }
            else {
                intersection(DFVInInxn, DFVIn, DFVTmp);
            }
            DFVInInxn = DFVTmp;
        }
        for (auto EOut : get_output_edges(BB)) {
            DataFlowValue& DFVOut = ELA[EOut];
            DataFlowValue DFVNew = DFVOut;
            DFVNew.insert(DFVInInxn.begin(), DFVInInxn.end());
            if (DFVNew.size() > DFVOut.size()) {
                pass_did_work = true;
            }
            DFVOut = DFVNew;
        }
    }

    return pass_did_work;
}


bool bwd_inter(EdgeLevelAnalysis& ELA, const Function& F)
{
    bool pass_did_work = false;

    for (auto& BB : F) {
        DataFlowValue DFVOutInxn;
        bool first_set = true;
        for (auto EOut : get_output_edges(BB)) {
            DataFlowValue& DFVOut = ELA[EOut];
            DataFlowValue DFVTmp;
            if (first_set) {
                DFVTmp = DFVOut;
                first_set = false;
            }
            else {
                intersection(DFVOutInxn, DFVOut, DFVTmp);
            }
            DFVOutInxn = DFVTmp;
        }
        for (auto& I : BB) {
            DFVOutInxn.erase(&I);
        }
        for (auto EIn : get_input_edges(BB)) {
            DataFlowValue& DFVIn = ELA[EIn];
            DataFlowValue DFVNew = DFVIn;
            DFVNew.insert(DFVOutInxn.begin(), DFVOutInxn.end());
            if (DFVNew.size() > DFVIn.size()) {
                pass_did_work = true;
            }
            DFVIn = DFVNew;
        }
    }

    return pass_did_work;
}


bool inter_edge(EdgeLevelAnalysis& ELA, const Function& F) {
    return fwd_inter(ELA, F) || bwd_inter(ELA, F);
}


bool find_transmits(EdgeLevelAnalysis& ELA,
                    const Function& F,
                    ValueSet& transmitted_vals,
                    unordered_map<const Value*, BlockSet>& transmitter_locs)
{
    bool pass_did_work = false;

    for (auto& BB : F) {
        for (auto& I : BB) {
            if (!is_transmit(&I)) continue;
            pass_did_work = true;
            auto LeakedOp = leaked_op(I);
            for (auto E : get_output_edges(BB)) {
                ELA[E].insert(LeakedOp);
                transmitted_vals.insert(LeakedOp);
            }
            if (!transmitter_locs.contains(LeakedOp)) {
                transmitter_locs[LeakedOp] = {};
            }
            transmitter_locs[LeakedOp].insert(&BB);
        }
    }

    return pass_did_work;
}


bool fwd_phi_prop(EdgeLevelAnalysis& ELA, const Function& F)
{
    bool pass_did_work = false;
    for (auto& BB : F) {
        for (auto& I : BB) {
            auto Phi = dyn_cast<PHINode>(&I);
            if (!Phi) continue;
            auto num_incoming = Phi->getNumIncomingValues();
            bool all_decl = true;
            for (unsigned int i = 0; i < num_incoming; i++) {
                auto VIncoming = Phi->getIncomingValue(i);
                if (is_const_val(VIncoming)) {
                    continue;
                }
                auto BBIncoming = Phi->getIncomingBlock(i);
                CFGEdge EIncoming = make_pair(BBIncoming, &BB);
                auto& DFVIncoming = ELA[EIncoming];
                if (!DFVIncoming.contains(VIncoming)) {
                    all_decl = false;
                    break;
                }
            }
            if (!all_decl) continue;
            for (auto EOut : get_output_edges(BB)) {
                auto& DFVOut = ELA[EOut];
                if (!DFVOut.contains(Phi)) {
                    DFVOut.insert(Phi);
                    pass_did_work = true;
                }
            }
        }
    }
    return pass_did_work;
}


bool bwd_phi_prop(EdgeLevelAnalysis& ELA, const Function& F)
{
    bool pass_did_work = false;
    for (auto& BB : F) {
        for (auto& I : BB) {
            auto Phi = dyn_cast<PHINode>(&I);
            if (!Phi) continue;
            bool all_decl = true;
            for (auto EOut : get_output_edges(BB)) {
                auto& DFVOut = ELA[EOut];
                if (!DFVOut.contains(Phi)) {
                    all_decl = false;
                    break;
                }
            }
            if (!all_decl) continue;
            auto num_incoming = Phi->getNumIncomingValues();
            for (unsigned int i = 0; i < num_incoming; i++) {
                auto VIncoming = Phi->getIncomingValue(i);
                if (is_const_val(VIncoming)) {
                    continue;
                }
                auto BBIncoming = Phi->getIncomingBlock(i);
                CFGEdge EIncoming = make_pair(BBIncoming, &BB);
                auto& DFVIncoming = ELA[EIncoming];
                if (!DFVIncoming.contains(VIncoming)) {
                    DFVIncoming.insert(VIncoming);
                    pass_did_work = true;
                }
            }
        }
    }
    return pass_did_work;
}


bool phi_prop(EdgeLevelAnalysis& ELA, const Function& F) {
    return fwd_phi_prop(ELA, F) || bwd_phi_prop(ELA, F);
}


void knowledge_expansion(EdgeLevelAnalysis& ELA, const Function& F) {
    bool pass_did_work = true;

    while(pass_did_work) {
        pass_did_work = false;
        for (auto it = inst_begin(F); it != inst_end(F); ++it) {
            auto& I = *it;
            if (!is_dataflow_instr(&I)) continue;
            for (auto E : get_all_edges(F)) {
                DataFlowValue& DFV = ELA[E];
                pass_did_work |= fwd_transfer(DFV, I);
                pass_did_work |= bwd_transfer(DFV, I);
            }
        }
    }
}
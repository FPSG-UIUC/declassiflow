#include "loop.h"
#include "ANSI.h"
#include <assert.h>
#include <queue>

using namespace std;
using namespace llvm;

bool loop_condition_check(Function& F) {

    // simple loop check
    DominatorTree domTree = DominatorTree(F);
    for(auto& BB : F) {
        bool simple_loop = true;
        int block_backedge_counter = 0;
        for(auto* BBin : predecessors(&BB)) {
            if(domTree.dominates(&BB, BBin)) {
                block_backedge_counter++;
                PHINode* inductive_phi;
                if( !(inductive_phi = dyn_cast<PHINode>(&BB.front())) ) {
                    simple_loop = false;
                    break;
                }
                unsigned int values = inductive_phi->getNumIncomingValues();
                if(values != 2) {
                    simple_loop = false;
                    break;
                }
            }
        }
        if(block_backedge_counter > 1) simple_loop = 0;
        if(!simple_loop)
            return false;
    }
    return true;
}

LoopInfo get_all_loops(Function& F) {
    DominatorTree domTree = DominatorTree(F);
    LoopInfo LI = LoopInfo(domTree);
    return LI;
}

DefUseMap get_outgoing_values(Function& F, LoopInfo& LI) {
    DefUseMap DUM;
    for(Loop* loop : LI.getLoopsInPreorder())
        for(BasicBlock* BB : loop->getBlocksVector())
            for(Instruction& I : *BB)
                for(User* U : I.users()) {
                    Instruction *user_inst = dyn_cast<Instruction>(U);
                    if(user_inst && !loop->contains(user_inst->getParent())) {
                        if(DUM.find(&I) == DUM.end()) DUM[&I] = vector<User*>(0, nullptr);
                        DUM[&I].push_back(U);
                    }
                }
    return DUM;
}

ValueTreeMap expand_all_loops(Function& F, DefUseMap& DUM) {
    ValueTreeMap value_tree;
    LoopInfo LI = get_all_loops(F);
    while(LI.getLoopsInPreorder().size() > 0) {
        SmallVector<Loop*> loops = LI.getLoopsInPreorder();
        reverse(loops.begin(), loops.end());
        expand_loop(F, loops[0], DUM, value_tree);
        LI = get_all_loops(F);
    }
    return value_tree;
}

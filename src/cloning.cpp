#include "cloning.h"
#include "ANSI.h"
#include <assert.h>

using namespace std;
using namespace llvm;

// cloning predeclarations
BasicBlock* MyCloneBasicBlock(const BasicBlock *BB, ValueToValueMapTy &VMap,
                                    const Twine &NameSuffix, Function *F);
BlockSet clone_loop(Function& F, Loop* loop, ValueToValueMapTy& block_map, ValueToValueMapTy& inst_map);
void remove_bad_duplicated_blocks(Function& F, Loop* loop, ValueToValueMapTy& block_map,
                                  SmallVector<BasicBlock*>& exiting_blocks, BasicBlock* loop_latch);
void connect_loop_expansion(Function& F, Loop* loop, ValueToValueMapTy& block_map);

// merge node predeclarations
void setup_merge_nodes(Function& F, Loop* loop,
                       SmallVector<BasicBlock*>& exit_blocks,
                       SmallVector<BasicBlock*>& exiting_blocks,
                       ValueToValueMapTy& block_map, ValueToValueMapTy& inst_map,
                       DefUseMap& DUM, DominatorTree& original_dom_tree, IRBuilder<>& builder,
                       BlockSet& full_expanded_loop_block_set);
void fill_merge_nodes(Function& F, MergeNodeMap& merge_node_map,
                             SmallVector<BasicBlock*>& exiting_blocks,
                             ValueToValueMapTy& block_map, ValueToValueMapTy& inst_map,
                             DefUseMap& DUM, DominatorTree& original_dom_tree, IRBuilder<>& builder,
                             BlockSet& loop_block_set);
MergeNodeMap create_merge_nodes(Function& F, SmallVector<BasicBlock*>& exiting_blocks,
                                ValueToValueMapTy& block_map, IRBuilder<>& builder, BlockSet& loop_block_set);
void repair_exit_phi_nodes(Loop* loop, SmallVector<BasicBlock*>& exit_blocks, MergeNodeMap& merge_node_map);
vector<BasicBlock*> connect_exiting_block_to_merge_node(BlockSet& block_set, BasicBlock* bb_to_connect,
                                                                   BasicBlock* merge_node);

// utility predeclarations
void add_to_value_tree_map(ValueTreeMap& value_tree, ValueToValueMapTy& value_mapping);
BlockSet create_block_set(ValueToValueMapTy& block_map);
Value* phi_remove_all_except_bb(PHINode* phi, BasicBlock* BB);


/*
#####################################################################
#####################################################################
#                                 MAIN                              #
#####################################################################
#####################################################################
*/
/*
    Returns the original values in the function F. This includes the original basic blocks and the original instructions
    and returns the collection of such values in the ValueSet <original_values>
*/
ValueSet get_original_values(Function& F) {
    ValueSet original_values;
    for(BasicBlock& BB : F) {
        original_values.insert(dyn_cast<Value>(&BB));
        for(Instruction& I : BB)
            original_values.insert(dyn_cast<Value>(&I));
    }
    for (auto& arg : F.args()) original_values.insert(dyn_cast<Value>(&arg)); // also need to add in function arguments
    return original_values;
}

/*
    This function expands the given LLVM Loop <loop> the same way that is done in the declassiflow paper. It also adds to the
    <value_tree> to map added values to the original value for the analysis results.
*/
void expand_loop(Function& F, Loop* loop, DefUseMap& DUM, ValueTreeMap& value_tree) {
    // original dominator tree
    DominatorTree original_dom_tree = DominatorTree(F);

    // get exit, exiting nodes, and loop latch (for BB1)
    BasicBlock* loop_latch = loop->getLoopLatch();
    SmallVector<BasicBlock*> exit_blocks = SmallVector<BasicBlock*>(0);
    SmallVector<BasicBlock*> exiting_blocks = SmallVector<BasicBlock*>(0);
    loop->getExitBlocks(exit_blocks);
    loop->getExitingBlocks(exiting_blocks);

    // clone the loop
    ValueToValueMapTy block_map, inst_map;
    BlockSet cloned_loop_set = clone_loop(F, loop, block_map, inst_map);
    BlockSet full_expanded_loop_block_set = create_block_set(block_map);

    // stich loops together
    connect_loop_expansion(F, loop, block_map);

    // Remove bad latch nodes from duplicated loop (note if this is before stich_loops, stich_loops will break since it expects the latch node)
    remove_bad_duplicated_blocks(F, loop, block_map, exiting_blocks, loop_latch);

    // populate value tree
    add_to_value_tree_map(value_tree, inst_map);
    add_to_value_tree_map(value_tree, block_map);

    // create IR builder
    LLVMContext& ctx = F.getContext();
    IRBuilder<> builder(ctx);

    // add merge nodes
    setup_merge_nodes(F, loop, exit_blocks, exiting_blocks, block_map, inst_map, DUM, original_dom_tree, builder, full_expanded_loop_block_set);
}


/*
#####################################################################
#####################################################################
#                               CLONING                             #
#####################################################################
#####################################################################
*/
// original function: https://llvm.org/doxygen/CloneFunction_8cpp_source.html (cloning basicblock source code) (PROVIDES MAPPING FROM OLD INSTRUCTION TO CLONED ONE!!!)
/*
    This clones a basic block <BB> while also remapping the cloned basic block instructions to use values from the current and previous
    cloned basic blocks stored in the ValueToValueMap <VMap>.

    Return Value:
    Returns a pointer to the cloned basic block
*/
BasicBlock* MyCloneBasicBlock(const BasicBlock *BB, ValueToValueMapTy &VMap,
                                    const Twine &NameSuffix, Function *F)
{
    BasicBlock* NewBB = BasicBlock::Create(BB->getContext(), "", F);
    if (BB->hasName())
        NewBB->setName(BB->getName() + NameSuffix);

    // Loop over all instructions, and copy them over.
    Instruction* PrevNewInst = nullptr;
    for (const Instruction& I : *BB) {
        Instruction *NewInst = I.clone();
        if (I.hasName()) {
            NewInst->setName(I.getName() + NameSuffix);
        }
        if (PrevNewInst != nullptr) {
            NewInst->insertAfter(PrevNewInst);
        }
        else {
            NewInst->insertInto(NewBB, NewBB->begin());
        }
        PrevNewInst = NewInst;
        VMap[&I] = NewInst; // Add instruction map to value.
    }

    // loop over all instructions and remap operands
    for(const Instruction& I : *BB) {
        Instruction *NewInst = dyn_cast<Instruction>(VMap[&I]);
        for(unsigned i = 0; i < NewInst->getNumOperands(); i++) {
            Value *op_cur = NewInst->getOperand(i);
            if(VMap.find(op_cur) != VMap.end())  // need to point to cloned instruction
                NewInst->setOperand(i, VMap[op_cur]);
        }
    }

    return NewBB;
}

/*
    This clones the entire sequence of basic blocks contained in the LLVM Loop <loop>. This includes changing branch pointers and
    argument values to the cloned sequence of pointers and values in <block_map> and <inst_map>.

    Return Values:
    This function will return a fully populated block mapping from original to cloned in <block_map> and the same for
    the instruction map <inst_map>. It will also return a BlockSet <bb_2_set> containing all blocks that are in the cloned basic
    block set.
*/
BlockSet clone_loop(Function& F, Loop* loop, ValueToValueMapTy& block_map, ValueToValueMapTy& inst_map) {
    // clone each basic block in loop
    BlockSet bb_2_set;  // stores all the bb_2's
    for(BasicBlock* BB_1 : loop->getBlocksVector()) {
        BasicBlock* BB_2 = MyCloneBasicBlock(BB_1, inst_map, "", &F);
        block_map[BB_1] = BB_2;
        bb_2_set.insert(BB_2);
    }

    // Change all successors
    for(BasicBlock* BB_1 : loop->getBlocksVector()) {
        // replace non phi node users
        BasicBlock* BB_2 = dyn_cast<BasicBlock>(block_map[BB_1]);
        vector<User*> user_to_fix;
        for(User* U : BB_1->users()) { // see if any cloned blocks
            Instruction* I = dyn_cast<Instruction>(U);
            if(I && bb_2_set.find(I->getParent()) != bb_2_set.end())  // cloned block has bad user
                user_to_fix.push_back(U);
        }

        // replace bad branch instructions
        for(User* U : user_to_fix)
            U->replaceUsesOfWith(BB_1, BB_2);

        // replace bad phi node users
        for(const BasicBlock* bb : bb_2_set)
            ((BasicBlock*)bb)->replacePhiUsesWith(BB_1, BB_2);
    }
    return bb_2_set;
}

// TODO reformat
/*
    This will remove unreached basic blocks that might cause unsoundness in the analysis for the cloned blocks. It will
    also reorganize block pointers so that the correct exit behavior in the cloned loop is correct
*/
void remove_bad_duplicated_blocks(Function& F, Loop* loop, ValueToValueMapTy& block_map,
                                  SmallVector<BasicBlock*>& exiting_blocks, BasicBlock* loop_latch) {
    queue<BasicBlock*> next;
    next.push(loop_latch);
    vector<BasicBlock*> exiting_blocks_to_repair;
    vector<BasicBlock*> removed_blocks;
    while(!next.empty()) {
        // get basic block information
        BasicBlock* BB_1 = next.front(); next.pop();
        BasicBlock* BB_2 = dyn_cast<BasicBlock>(block_map[BB_1]);

        // if exiting we stop deletion
        if(find(exiting_blocks.begin(), exiting_blocks.end(), BB_1) != exiting_blocks.end()) {
            exiting_blocks_to_repair.push_back(BB_2);
            continue;
        }
        else if(loop->contains(BB_1) == false) {
            errs() << "This should not happen!\n";
            exit(1);
        }
        else if(BB_1 == loop->getHeader()) {
            errs() << "Infinite loop? Got to the header without finding exiting node\n";
            exit(1);
        }

        // delete it if not
        removed_blocks.push_back(BB_2);
        BB_2->eraseFromParent();

        // add predecessors
        for(BasicBlock* BBin : predecessors(BB_1)) next.push(BBin);
    }

    // repair exiting blocks
    // you can remove by using the
    for(BasicBlock* bb_2_exiting : exiting_blocks_to_repair) {
        BranchInst* bb_br = dyn_cast<BranchInst>(&(bb_2_exiting->back()));

        // find the exit block
        BasicBlock* exit_bb;
        BasicBlock* removed_bb;
        if(bb_br->getNumSuccessors() == 2) {
            if(find(removed_blocks.begin(), removed_blocks.end(), bb_br->getSuccessor(0)) == removed_blocks.end()) {
                exit_bb = bb_br->getSuccessor(0);
                removed_bb = bb_br->getSuccessor(1);
            }
            else {
                exit_bb = bb_br->getSuccessor(1);
                removed_bb = bb_br->getSuccessor(0);
            }
        }
        else {
            assert(false); // not possible, somehow needed to repair an exiting node that isn't reachable from latch (also shouldn't be a part of the loop)
            exit_bb = bb_br->getSuccessor(0);
            removed_bb = nullptr;
        }

        // replace
        bb_br->replaceUsesOfWith(removed_bb, exit_bb);
    }
}

/*
    This function will connect the original loop <loop> with the cloned loop (mapping in <block_map>) in the way that is
    described in the declassiflow paper.
*/
void connect_loop_expansion(Function& F, Loop* loop, ValueToValueMapTy& block_map) {
    // Stich together loops
    BasicBlock* bb_1_header = loop->getHeader();
    BasicBlock* bb_2_header = dyn_cast<BasicBlock>(block_map[bb_1_header]);

    // modify original loop
    Value* bb_2_phi_v = nullptr;
    BasicBlock* bb_1_latch = loop->getLoopLatch();  // bb_1 header start
    bb_1_latch->back().replaceUsesOfWith(bb_1_header, bb_2_header);

    // go till run out of phis
    PHINode* bb_1_inductive_phi = nullptr;
    auto bb_1_ptr = bb_1_header->begin();
    auto bb_2_ptr = bb_2_header->begin();
    while( (bb_1_inductive_phi = dyn_cast<PHINode>( &*bb_1_ptr ))) {
        // BB_1 inductive phi
        bb_2_phi_v = phi_remove_all_except_bb(bb_1_inductive_phi, loop->getLoopPredecessor());

        // modify cloned loop
        PHINode* bb_2_inductive_phi = dyn_cast<PHINode>( &*bb_2_ptr ); // bb_2 latch start TODO find better way to remove const
        bb_2_inductive_phi->removeIncomingValue(loop->getLoopPredecessor());
        bb_2_inductive_phi->setIncomingBlock(0, bb_1_latch);
        bb_2_inductive_phi->setIncomingValue(0, bb_2_phi_v);
        bb_1_ptr++;
        bb_2_ptr++;
    }

    // fix backedge in loop2
    BasicBlock* bb_2_latch = dyn_cast<BasicBlock>(block_map[bb_1_latch]);
    BranchInst* bb_2_br = dyn_cast<BranchInst>(&bb_2_latch->back());
    if(bb_2_br->getNumSuccessors() == 2) { // no need to run this if the latch is not an exiting node
        BasicBlock* exit_block;
        for(unsigned i = 0; i < bb_2_br->getNumSuccessors(); i++)
            if(bb_2_br->getSuccessor(i) != bb_2_header) {
                exit_block = bb_2_br->getSuccessor(i);
            }
        bb_2_br->replaceUsesOfWith(bb_2_header, exit_block);
    }
    else { // assert that latch points to the header
        assert(bb_2_br->getSuccessor(0) == bb_2_header);
    }
}


/*
#####################################################################
#####################################################################
#                               MERGE                              #
#####################################################################
#####################################################################
*/
/*
    Creates merge nodes for all exiting blocks and repairs all connections using the old basic blocks (non-merge node blocks)
*/
void setup_merge_nodes(Function& F, Loop* loop,
                       SmallVector<BasicBlock*>& exit_blocks,
                       SmallVector<BasicBlock*>& exiting_blocks,
                       ValueToValueMapTy& block_map, ValueToValueMapTy& inst_map,
                       DefUseMap& DUM, DominatorTree& original_dom_tree, IRBuilder<>& builder,
                       BlockSet& full_expanded_loop_block_set) {
    // Create merge node blocks
    MergeNodeMap merge_node_map = create_merge_nodes(F, exiting_blocks, block_map, builder, full_expanded_loop_block_set);

    // add in phis
    fill_merge_nodes(F, merge_node_map, exiting_blocks, block_map, inst_map, DUM, original_dom_tree, builder, full_expanded_loop_block_set);

    // repair the exit phi nodes
    repair_exit_phi_nodes(loop, exit_blocks, merge_node_map);
}

/*
    This function will add phis to the merge nodes to connect the cloned value and the original value if the values are used
    outside the loop. This allows for the correct propagation of values in accordance with the declassiflow paper.
*/
void fill_merge_nodes(Function& F, MergeNodeMap& merge_node_map,
                             SmallVector<BasicBlock*>& exiting_blocks,
                             ValueToValueMapTy& block_map, ValueToValueMapTy& inst_map,
                             DefUseMap& DUM, DominatorTree& original_dom_tree, IRBuilder<>& builder,
                             BlockSet& loop_block_set) {
    for(BasicBlock* BB1 : exiting_blocks) {
        // init
        BasicBlock* BB2 = dyn_cast<BasicBlock>(block_map[BB1]);
        BasicBlock* merge_node = merge_node_map[BB1];
        builder.SetInsertPoint(&(merge_node->front()));

        // for each def use value
        for(const auto& [I1, v_users] : DUM) {
            // only values in the loop matter
            if(loop_block_set.find(I1->getParent()) == loop_block_set.end()) continue;

            // check to see if the usage is dominated by the current block
            if(original_dom_tree.dominates(I1->getParent(), BB1)) {
                Instruction* I2 = dyn_cast<Instruction>(inst_map[I1]);

                // create a phi for this
                Type* inst_type = I1->getType();
                PHINode* inst_phi = builder.CreatePHI(inst_type, 2);
                for(BasicBlock* bb_prev : predecessors(merge_node))
                    if(bb_prev == BB1)
                        inst_phi->addIncoming(I1, BB1);
                    else if(bb_prev == BB2)
                        inst_phi->addIncoming(I2, BB2);
                    else
                        assert(bb_prev == BB1 || bb_prev == BB2); // will cause error!

                // link all usages with the new phi
                // if user is a phi then only replace the use with the new use if the basicblock comes from the correct exiting block!
                // need to scan phi operands, if the operand matches I1 from block BB1 we replace
                // this strategy will prevent clashing between different merge nodes
                for(User* U : v_users) {
                    PHINode* phi;
                    if( (phi = dyn_cast<PHINode>(U)) ) {
                        for(unsigned i = 0; i < phi->getNumIncomingValues(); i++)
                            if(phi->getIncomingValue(i) == I1 && phi->getIncomingBlock(i) == BB1)
                                phi->setIncomingValue(i, dyn_cast<Value>(inst_phi));
                    }
                    else
                        U->replaceUsesOfWith(I1, inst_phi);
                }
            }
        }
    }
}

/*
    Creates a merge node for every exiting_block in both loops (original and cloned). This merge node is then mapped back to the
    respective exiting blocks in a MergeNodeMap structure.
*/
MergeNodeMap create_merge_nodes(Function& F, SmallVector<BasicBlock*>& exiting_blocks,
                                ValueToValueMapTy& block_map, IRBuilder<>& builder, BlockSet& loop_block_set) {
    MergeNodeMap merge_node_map;
    LLVMContext& ctx = F.getContext();

    for(BasicBlock* BB1 : exiting_blocks) {
        BasicBlock* BB2 = dyn_cast<BasicBlock>(block_map[BB1]);

        // create merge node
        BasicBlock* merge_node = BasicBlock::Create(ctx, "", &F, nullptr);
        merge_node_map[BB1] = merge_node;
        merge_node_map[BB2] = merge_node;

        // connect to merge node
        vector<BasicBlock*> replaced_in_bb1 = connect_exiting_block_to_merge_node(loop_block_set, BB1, merge_node);
        vector<BasicBlock*> replaced_in_bb2 = connect_exiting_block_to_merge_node(loop_block_set, BB2, merge_node);

        // assertions
        assert(replaced_in_bb1.size() == 1);
        assert(replaced_in_bb2.size() == 1 || (replaced_in_bb2.size() == 2 && replaced_in_bb2[0] == replaced_in_bb2[1]));
        assert(replaced_in_bb1[0] == replaced_in_bb2[0]);
        BasicBlock* exit_node = replaced_in_bb1[0];

        // add in branch
        builder.SetInsertPoint(merge_node);
        builder.CreateBr(exit_node);
    }

    return merge_node_map;
}

/*
    This function will look inside the exit blocks and check for phi instructions which have incoming basic blocks from exiting
    blocks that have been replaced with merge nodes and fixes it by changing it to the correct merge node instance
*/
void repair_exit_phi_nodes(Loop* loop, SmallVector<BasicBlock*>& exit_blocks, MergeNodeMap& merge_node_map) {
    // repair phinodes
    for(BasicBlock* exit_node : exit_blocks) {
            for(Instruction& I : *exit_node) {
                PHINode* phi_i = dyn_cast<PHINode>(&I);
                if(phi_i) {
                    for(unsigned i = 0; i < phi_i->getNumIncomingValues(); i++) {
                        BasicBlock* incoming_bb = phi_i->getIncomingBlock(i);
                        if(merge_node_map.find(incoming_bb) != merge_node_map.end())
                            phi_i->setIncomingBlock(i, merge_node_map[incoming_bb]);
                    }
                }
            }
    }
}

/*
    This function will connnect <bb_to_connect> to a merge node <merge_ndoe>. It does so by overwriting any connections to any
    block that isn't in the original loop block set or cloned loop block set <block_set>.

    Note: if <block_set> is empty all points would be considered outside the loops and would be replaced

    Return value:
    It then returns the basic block connections that were overwritten in this function.
*/
vector<BasicBlock*> connect_exiting_block_to_merge_node(BlockSet& block_set, BasicBlock* bb_to_connect,
                                                                   BasicBlock* merge_node) {
    vector<BasicBlock*> replaced_blocks;
    User* bb_br = dyn_cast<User>(&bb_to_connect->back());
    for(BasicBlock* bb_next : successors(bb_to_connect)) {
        if(block_set.find(bb_next) == block_set.end() && bb_next != merge_node) {
            bb_br->replaceUsesOfWith(bb_next, merge_node);
            replaced_blocks.push_back(bb_next);
        }
    }
    return replaced_blocks;
}


/*
#####################################################################
#####################################################################
#                               UTILITY                             #
#####################################################################
#####################################################################
*/
/*
    This is a utility function that will add values from the ValueToValueMap <value_mapping> to the ValueTreeMap <value_tree>.
*/
void add_to_value_tree_map(ValueTreeMap& value_tree, ValueToValueMapTy& value_mapping) {
    for(const auto& [key, value] : value_mapping) {
        if(value_tree.find(key) == value_tree.end())
            value_tree[key] = {};
        if(value)
            value_tree[key].insert(value);
    }
}

/*
    This function given the ValueToValueMap <block_map> returns a BlockSet containing the entire set of basicblocks in this mapping.
*/
BlockSet create_block_set(ValueToValueMapTy& block_map) {
    BlockSet ret;
    for(const auto& [BB1, BB2] : block_map) {
        ret.insert(dyn_cast<BasicBlock>(BB1));
        ret.insert(dyn_cast<BasicBlock>(BB2));
    }
    return ret;
}

/*
    Remove all arguments from PHINode <phi> except for the values that came from the incoming basic block <BB>
*/
Value* phi_remove_all_except_bb(PHINode* phi, BasicBlock* BB) {
    Value* ret = nullptr;
    for(unsigned i = 0; i < phi->getNumIncomingValues(); i++) {
        if(dyn_cast<BasicBlock>(phi->getIncomingBlock(i)) != BB) {
            ret = phi->getIncomingValue(i);
            phi->removeIncomingValue(i);
        }
    }
    return ret; // should only return once if truely a simple loop
}
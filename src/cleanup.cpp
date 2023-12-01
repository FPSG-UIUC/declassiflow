#include "cleanup.h"
#include "ANSI.h"
#include "llvm/IR/Dominators.h"

#include <queue>

using namespace llvm;
using namespace std;

void reduce(const EdgeLevelAnalysis& ELA,
            const Function& F,
            BlockLevelAnalysis& BLA)
{
    for (auto& BB : F) {
        bool first_set = true;
        DataFlowValue DFVDecl;
        for (auto EOut : get_output_edges(BB)) {
            auto DFV = ELA.at(EOut);
            DataFlowValue DFVTmp;
            if (first_set) {
                DFVTmp = DFV;
                first_set = false;
            }
            else {
                intersection(DFVDecl, DFV, DFVTmp);
            }
            DFVDecl = DFVTmp;
        }
        BLA[&BB] = DFVDecl;
    }
}


void loop_reduce(BlockLevelAnalysis& BLA,
                 const Function& F,
                 ValueSet& original_values)
{
    for(auto& BB : F) {
        if(original_values.find(&BB) == original_values.end()) {
            BLA.erase(&BB);
            continue;
        }
        DataFlowValue& DFAold = BLA[&BB];
        DataFlowValue DFAnew;
        intersection(DFAold, original_values, DFAnew);
        BLA[&BB] = DFAnew;
    }
}


void loop_map(BlockLevelAnalysis& BLA,
              const Function& F,
              ValueTreeMap& value_tree,
              ValueSet& original_values)
{
    for(auto& BB : F) {
        if(original_values.find(&BB) == original_values.end()) continue;

        DataFlowValue DFVNew;
        for(const Value* I : BLA[&BB])  {
            if(original_values.find(I) == original_values.end() || dyn_cast<Argument>(I)) { // if function argument directly add
                DFVNew.insert(I);
                continue;
            }
            // fetch sibling blocks
            ValueSet sibling_blocks = value_tree_children_of(value_tree, dyn_cast<Value>(dyn_cast<Instruction>(I)->getParent()));

            DataFlowValue sibling_values = value_tree_children_of(value_tree, dyn_cast<Value>(I));
            DataFlowValue sibling_values_existing;

            // take intersection of sibling_blocks with the sibling values
            for(const Value* sibling_bb : sibling_blocks)
                intersection(BLA[dyn_cast<BasicBlock>(sibling_bb)], sibling_values, sibling_values_existing);

            if(sibling_values.size() == sibling_values_existing.size())
                DFVNew.insert(I);
        }

        BLA[&BB] = DFVNew;
    }
}


KnowledgeFrontierMap knowledge_frontier(Function& F,
                                        const BlockLevelAnalysis& BLA)
{
    KnowledgeFrontierMap initial_frontier;
    for(auto& BB : F) {
        if (!BLA.contains(&BB)) continue;
        for(auto V : BLA.at(&BB)) {
            if(!initial_frontier.contains(V)) {
                initial_frontier[V] = {};
            }
            initial_frontier[V].insert(&BB);
        }
    }

    DominatorTree dom_tree(F);
    KnowledgeFrontierMap removals;
    for (auto pair : initial_frontier) {
        auto V = pair.first;
        if (!removals.contains(V)) {
            removals[V] = {};
        }
        auto& frontier = pair.second;
        for (auto BB1 : frontier) {
            for (auto BB2 : frontier) {
                if (BB1 != BB2 && dom_tree.dominates(BB1, BB2)) {
                    removals[V].insert(BB2);
                }
            }
        }
    }

    KnowledgeFrontierMap frontier_map;
    for (auto pair : initial_frontier) {
        auto V = pair.first;
        if (!frontier_map.contains(V)) {
            frontier_map[V] = {};
        }
        auto& frontier = pair.second;
        for (auto BB : frontier) {
            if (!removals[V].contains(BB)) {
                frontier_map[V].insert(BB);
            }
        }
    }

    return frontier_map;
}

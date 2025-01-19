use "trees/tree.sml";
use "trees/tree1.sml";
use "trees/tree2.sml";
use "trees/tree3.sml";
use "trees/tree4.sml";
use "trees/tree5.sml";

structure MyTreeStruct : TREE =
  struct
    type treeAVType = int
    type treeKeyType = int
    type treeValueType = int

    datatype augVal = AV of treeAVType | Null
    datatype elem   = AVContainer of ((treeKeyType * treeValueType * int) * treeAVType) 
                    | Container of ((treeKeyType * treeValueType * int));
    datatype tree   = Node of tree * elem * tree | Empty;

    val limit = 4

    fun baseAV (k: treeKeyType, v: treeValueType, b: int) : augVal = 
      AV (k*v)

    fun combineAV (Null, Null) : augVal = Null
      | combineAV (AV (av1), Null) : augVal = AV (av1)
      | combineAV (Null, AV (av2)) : augVal = AV (av2)
      | combineAV (AV (av1), AV (av2)) : augVal = AV (av1 + av2)
  end;

(* Create treap-based tree structure *)
structure x1 = MakeTree1(MyTreeStruct);
structure x2 = AddAVFSizeCheck(x1);
structure x3 = AddRotates(x2);
structure x4 = AddJoinTreap(x3);
structure x5 = AddFinalFunctions(x4);
open x5;

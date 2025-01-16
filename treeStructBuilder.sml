use "trees/tree.sml";
use "trees/tree1.sml";
use "trees/tree2.sml";
use "trees/tree3.sml";
use "trees/tree4.sml";
use "trees/tree5.sml";

functor MakeTreeStruct (Tin: TREE) : TREE5 =
  let
    exception UnavailableCaseException of string
  
    structure T1: TREE1 = MakeTree1 (Tin)
    fun T2 (T1': TREE1): TREE2 = (case Tin.avi of
                  AVF => AddAVFSizeCheck(T1)
                | MP => AddMaxPathCheck(T1)
                | T => AddAlwaysTrueCheck(T1)
                | F => AddAlwaysFalseCheck(T1))
    structure T3: TREE3 = AddRotates(T2(T1))
    fun T4 (T3': TREE3): TREE4 = (case Tin.ti of
                   RB => AddJoinRB(T3)
                 | _ => raise UnavailableCaseException "Haven't hade this bst invariant yet...")
  in
    AddFinalFunctions(T4(T3)) 
  end

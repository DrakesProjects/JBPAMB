signature TREE = 
  sig
    (*type of augmented value*)
    type treeAVType
    (*type of value stored with each key*)
    type treeValueType
    

    datatype augVal = AV of treeAVType | Null
    (* holds all of the data at each node, aside from subtrees*)
    datatype elem   = AVContainer of ((int * treeValueType * int) * treeAVType)
                    | Container of ((int * treeValueType * int));
    (* datatype for a tree *)
    datatype tree   = Node of tree * elem * tree | Empty;
    
    (* used in the creation of some versions of AVCheck as the
    * threshold for creating an augmented value at a given node*)
    val limit        : int
    (* see PAM: Parallel Augmented Maps *)
    val baseAV       : (int * treeValueType * int) -> augVal
    (* see PAM: Parallel Augmented Maps *)
    val combineAV    : augVal * augVal -> augVal
   

  end;


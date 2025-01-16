(*I used 'open' here in the fuctors I defined. This was because the intention of
        * these functors is simply to add a function to the structure TREE, It
        * seemed kind of redundant to redefine all of the functions from TREE in
        * the functors if I can just use 'open' to pull in the functions
        * automatically, but i'm not sure if this use of the syntax is okay...*)

(*MakeTree uses the subfunction avCheck to determine if a new augmented value is
* needed. We test different implementations of avCheck*)
signature TREE2 =
  sig
    type treeAVType
    type treeValueType

    datatype augVal = AV of treeAVType | Null
    datatype elem   = AVContainer of ((int * treeValueType * int) * treeAVType)
                    | Container of ((int * treeValueType * int));
    datatype tree   = Node of tree * elem * tree | Empty;

    val limit        : int
    val baseAV       : (int * treeValueType * int) -> augVal
    val combineAV    : augVal * augVal -> augVal
    (**)
    val getLChild    : tree -> tree
    val getRChild    : tree -> tree
    val getElem      : tree -> elem
    val hasAV        : tree -> bool
    val getAV        : tree -> augVal
    val concatTree   : tree * (int * treeValueType * int) * tree -> tree
    val concatTreeAV : tree * (int * treeValueType * int) * tree -> tree
    (**)
    (* determines if an augmented value is needed for a node with the two input
    * trees as children*)
    val avCheck      : tree * tree -> bool
  end;

(*Based on the length of the longest path from current node to either an 
* augmented value, or a leaf (which has minimal AV retrieval time*)
functor AddMaxPathCheck (T : TREE1) : TREE2 = 
  struct    
    open T;

    fun avCheck(T3: tree, T4: tree) : bool =
      let
        fun maxPathLength (T5: tree, T6: tree) =
          let
            val v1 = (case hasAV(T5) of
              true => 0
            | false => 1 + maxPathLength (getLChild(T5), getRChild(T5)))
            val v2 = (case hasAV(T6) of
              true => 0
            | false => 1 + maxPathLength (getLChild(T6), getRChild(T6)))
          in
            Int.max(v1, v2)
          end;
      in
        if (maxPathLength (T3, T4) > limit) then true else false
      end;
  end;

(*Based on the total size of the entire AV frontier, or in other words the 
* total number of nodes in every simple path down from the current node to 
* an augmented value or a leaf.*)
functor AddAVFSizeCheck (T : TREE1) : TREE2 = 
  struct    
    open T;

    fun avCheck(T3: tree, T4: tree) : bool =
      let
        fun avFrontierSize (T5: tree, T6: tree) =
          let
            val v1 = (case hasAV(T5) of
              true => 0
            | false => 1 + avFrontierSize (getLChild(T5), getRChild(T5)))
            val v2 = (case hasAV(T6) of
              true => 0
            | false => 1 + avFrontierSize (getLChild(T6), getRChild(T6)))
          in
            v1 + v2
          end;
      in
        if (avFrontierSize (T3, T4) > limit) then true else false
      end;
  end;

(*Make AVCheck always return true, guranteeing that every value has an augmented
* value*)
functor AddAlwaysTrueCheck (T : TREE1) : TREE2 =
  struct
    open T;

    fun avCheck(T3: tree, T4: tree) : bool = true
  end;

(*Make AVCheck always return false, meaning no node has an augmented value*)
functor AddAlwaysFalseCheck (T : TREE1) : TREE2 =
  struct
    open T;

    fun avCheck(T3: tree, T4: tree) : bool = false
  end;

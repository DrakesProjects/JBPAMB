signature TREE1 = 
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
    (*checks if a tree as an augmented value at the root node*)
    val hasAV        : tree -> bool
    (*gets the augmented value stored at the root node or returns null if empty*)
    val getAV        : tree -> augVal
    (*simply creates a tree with the given subtrees and tuple with no added
    * operations and no augmented value*)
    val concatTree   : tree * (int * treeValueType * int) * tree -> tree
    (*same as concatTree but creates the tree with an augmented value*)
    val concatTreeAV : tree * (int * treeValueType * int) * tree -> tree
  end;

functor MakeTree1 (T: TREE) : TREE1 =
  struct
    open T;

    exception EmptyNodeException of string
    exception CombineException of string

    fun getLChild Empty = raise EmptyNodeException "Cannot get left child of an empty node" 
      | getLChild (Node (lChild, _, _)) = lChild

    fun getRChild Empty = raise EmptyNodeException "Cannot get right child of an empty node"
      | getRChild (Node (_, _, rChild)) = rChild

    fun getElem Empty = raise EmptyNodeException "Cannot access contents of an empty node"
      | getElem (Node (_, e1, _)) = e1

    fun hasAV Empty = true 
      | hasAV (Node (_, e1, _)) =
      (case e1 of
           (AVContainer((_, _, _), _)) => true
         | _ => false)

    fun getAV Empty : augVal = Null
      | getAV (Node (L1, e1, R1)) : augVal =
      (case e1 of
           (AVContainer((_, _, _), augval)) => AV (augval)
         | (Container (k, v, b)) => combineAV (combineAV (getAV(L1), baseAV(k, v, b)), getAV(R1)))

    fun concatTree (T1: tree, (k: int, v: treeValueType, b: int), T2: tree) : tree =
      let
        val e1 = Container((k, v, b))
      in
        Node(T1, e1, T2)
      end

    fun concatTreeAV (T1: tree, (k: int, v: treeValueType, b: int), T2: tree) : tree =
      let
        val av = (case combineAV(combineAV(getAV(T1), baseAV(k, v, b)), getAV(T2)) of
              (AV (av1)) => av1
            | (Null) => raise CombineException "Error performing combine within concatTreeAV")
        val e1 = AVContainer((k, v, b), av)
      in
        Node(T1, e1, T2)
      end
  end;


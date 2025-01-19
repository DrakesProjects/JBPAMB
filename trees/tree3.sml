signature TREE3 =
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
    val avCheck      : tree * tree -> bool
    (**)
    (*this should be used for making trees. Uses AVCheck to determine if
    * concatTree or concatTreeAV should be used to create the tree*)
    val makeTree     : tree * (int * treeValueType * int) * tree -> tree
    val rotateLeft   : tree -> tree
    val rotateRight  : tree -> tree
  end;

functor AddRotates (T: TREE2) : TREE3 =
  struct
    open T

    exception EmptyNodeException of string

    fun makeTree(T1: tree, (k1: int, v1: treeValueType, b1: int), T2: tree) =
      if (avCheck(T1, T2)) then
        concatTreeAV (T1, (k1, v1, b1), T2)
      else 
        concatTree (T1, (k1, v1, b1), T2)

    fun rotateLeft Empty = raise EmptyNodeException "Cannot rotate left on an empty node" 
      | rotateLeft (Node (lChild, e1, rChild)) =
       case rChild of
            Empty => raise EmptyNodeException "Cannot rotate left when right child is empty"
          | (Node (rlChild, e2, rrChild)) =>
              let
                val x1 = (case e1 of
                     (AVContainer ((k1, v1, b1), _)) => (k1, v1, b1)
                   | (Container ((k1, v1, b1)))      => (k1, v1, b1))
                val x2 = (case e2 of
                     (AVContainer ((k1, v1, b1), _)) => (k1, v1, b1)
                   | (Container ((k1, v1, b1)))      => (k1, v1, b1))
                val T1 = makeTree(lChild, x1, rlChild)
              in
                makeTree(T1, x2, rrChild)
              end

    fun rotateRight Empty = raise EmptyNodeException "Cannot rotate left on an empty node" 
      | rotateRight (Node (lChild, e1, rChild)) =
       case lChild of
            Empty => raise EmptyNodeException "Cannot rotate left when right child is empty"
          | (Node (llChild, e2, lrChild)) =>
              let
                val x1 = (case e1 of
                     (AVContainer ((k1, v1, b1), _)) => (k1, v1, b1)
                   | (Container ((k1, v1, b1)))      => (k1, v1, b1))
                val x2 = (case e2 of
                     (AVContainer ((k1, v1, b1), _)) => (k1, v1, b1)
                   | (Container ((k1, v1, b1)))      => (k1, v1, b1))
                val T1 = makeTree(lrChild, x1, rChild)
              in
                makeTree(llChild, x2, T1)
              end
  end;



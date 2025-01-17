(*insert, delete, search*)
signature TREE5 =
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
    val makeTree     : tree * (int * treeValueType * int) * tree -> tree
    val rotateLeft   : tree -> tree
    val rotateRight  : tree -> tree
    (**)
    val singleton    : (int * treeValueType) -> tree
    val join         : tree * (int * treeValueType) * tree -> tree
    (**)
    val insert       : (tree * (int * treeValueType)) -> tree

    val getTreeSize            : tree -> int
    (*Ensures Lk < k < Rk at each node*)
    val bstInvariantCheck      : tree -> bool
    (* counts the total number of augmented values within the input tree*)
    val avCount                : tree -> int
    (* ensures that everywhyere avCheck is true, there is an augemented value
    * stored at that node*)
    val avCheckInvariantCheck  : tree -> bool

    (*Ensures no red node has red children and all black heights from the root
    * are equal*)
    val rbTreeInvariantCheck   : tree -> bool
    (*Ensures each node in the tree has weight-balanced subtrees*)
    val wbTreeInvariantCheck   : tree -> bool
    (*Ensures both that the each node accurately stores its height and also that
    * no two subtrees of a single node differ in height by more than 1*)
    val avlTreeInvariantCheck  : tree -> bool
    (*Ensures the heap property is satisfied*)
    val treapInvariantCheck    : tree -> bool
 
  end;

functor AddFinalFunctions(T: TREE4) : TREE5 = 
  struct
    open T;

    fun insert (T1: tree, (k1: int, v1: treeValueType)): tree =
      (case T1 of
            Empty => singleton (k1, v1)
          | (Node (L, Container (k2, v2, _), R)) =>
              if (k1 < k2) then join(insert(L, (k1, v1)), (k2, v2), R)
              else if (k1 > k2) then join(L, (k2, v2), insert(R, (k1, v1)))
              else join(L, (k1, v1), R)
          | (Node (L, AVContainer((k2, v2, _), _), R)) =>
              if (k1 < k2) then join(insert(L, (k1, v1)), (k2, v2), R)
              else if (k1 > k2) then join(L, (k2, v2), insert(R, (k1, v1)))
              else join(L, (k1, v1), R)
              )

    fun getTreeSize (Empty) = 0
      | getTreeSize (Node (L, _, R)) = 1 + getTreeSize(L) + getTreeSize(R)

    fun bstInvariantCheck (Empty): bool = true
      | bstInvariantCheck (Node (L, e, R)): bool =
      let
        val k = (case e of
                      Container((k', _, _)) => k'
                    | AVContainer((k', _, _), _) => k')
        val rInv = (case R of
              Empty => true
            | Node (RL, Container (Rk, _, _), RR) =>
                if (k < Rk) then (true andalso bstInvariantCheck(R))
                else false
            | Node (RL, AVContainer ((Rk, _, _), _), RR) =>
                if (k < Rk) then (true andalso bstInvariantCheck(R))
                else false
        )
        val lInv = (case L of
              Empty => true
            | Node (LL, Container (Lk, _, _), LR) =>
                if (k > Lk) then (true andalso bstInvariantCheck(L))
                else false
            | Node (LL, AVContainer ((Lk, _, _), _), LR) =>
                if (k > Lk) then (true andalso bstInvariantCheck(L))
                else false
        )
      in
        (rInv andalso lInv)
      end
 
    fun avCount (T1: tree): int =
      (case T1 of
           Empty => 0
         | Node (L, Container(_, _, _), R) => avCount(L) + avCount(R)
         | Node (L, AVContainer ((_, _, _), _), R) => 1 + avCount(L) + avCount(R))
 
    fun avCheckInvariantCheck (T1: tree): bool =
      (case T1 of
           Node (T1L, Container (_, _, _), T1R) => ((not (avCheck(T1L, T1R))) andalso
           (avCheckInvariantCheck(T1L) andalso avCheckInvariantCheck(T1R)))
         | _ => true)

    fun rbTreeInvariantCheck (T1: tree): bool =
      let
        val Red = 0
        val Black = 1

        fun getColor (Empty): int = Black
          | getColor (Node (_, AVContainer ((_, _, color), _), _)) = color
          | getColor (Node (_, Container (_, _, color), _)) = color
        
        fun blackHeightCounter(Empty, n: int): int = 1
          | blackHeightCounter(Node (L, e, R), n: int): int =
          let
            val x = (case e of
                  (Container (_, _, c)) => (L, c, R)
                | (AVContainer ((_, _, c), _)) => (L, c, R))
            val bhL = blackHeightCounter(#1(x), n)
          in
            if (bhL <> blackHeightCounter(#3(x), n)) then 0
            else bhL + (#2(x))
          end

      in
        if ((getColor(T1) = Red) andalso ((getColor(getLChild(T1)) = Red) orelse
        (getColor(getRChild(T1)) = Red))) then false
        else if (blackHeightCounter(T1, 0) > 0) then true
        else false
      end

    fun wbTreeInvariantCheck(Empty) = true
      | wbTreeInvariantCheck (T1: tree): bool =
      let
        val ALPHA = 0.29
        val BETA = 1.0 - ALPHA

        fun getWeight(T2: tree): int = getTreeSize(T2)

        fun balance (i1: int, i2: int): bool =
          let
            val isum = i1 + i2
          in
            if ((Real.fromInt(i1) / Real.fromInt(isum) < ALPHA) orelse
            (Real.fromInt(i1) / Real.fromInt(isum) > BETA)) then false
            else true
          end
      in
        (balance(getWeight(getLChild(T1)), getWeight(getRChild(T1))) andalso
        wbTreeInvariantCheck(getLChild(T1)) andalso 
        wbTreeInvariantCheck(getRChild(T1)))
      end

      fun avlTreeInvariantCheck(T1: tree): bool =
        let
          fun getH (Empty) = 0
            | getH (Node(_, Container(_, _, h), _)) = h
            | getH (Node(_, AVContainer((_, _, h), _), _)) = h

          fun getHeight (Empty) = 0
            | getHeight (Node (L1, _, R1)) =
            1 + Int.max(getHeight(L1), getHeight(R1))

          (*can't differ by more than 1*)
          fun avlInv (Empty) = true
            | avlInv (Node(T2L, _, T2R)): bool =
            (Int.abs(getHeight(T2L) - getHeight(T2R)) <= 1)

          val c1: bool = (getH(T1) = getHeight(T1))
          val c2: bool = avlInv(T1)
        in
          ((c1 andalso c2) andalso (avlTreeInvariantCheck(getLChild(T1)) andalso
          avlTreeInvariantCheck(getRChild(T1))))
        end

      fun treapInvariantCheck(Empty) = true
        | treapInvariantCheck(T1: tree): bool =
        let
          (*we change prior to use >= instead of > because this allows for two of
                          * the same priority to not trigger a false alarm*)
          fun prior(i1: int, i2: int): bool = i1 >= i2
          fun getP(T2: tree): int =
            (case T2 of
                  Empty => ~1
                | Node (_, Container(_, _, T2p), _) => T2p
                | Node (_, AVContainer ((_, _, T2p), _), _) => T2p)
        in
          if ((prior(getP(T1), getP(getLChild(T1)))) andalso (prior(getP(T1),
          getP(getRChild(T1)))) andalso (treapInvariantCheck(getLChild(T1)))
          andalso (treapInvariantCheck(getRChild(T1)))) then true
          else false
        end
  end;



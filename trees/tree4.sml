(*add joins here*)
signature TREE4 =
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
    datatype jInput = KVIn of (int * treeValueType) | KVPIn of (int * treeValueType * int);

    (*creates a tree with empty subtrees from the input key and value*)
    val singleton    : (int * treeValueType) -> tree
    (*See Just Join for Parallel Ordered Sets*)
    val join         : tree * jInput * tree -> tree
  end;

functor AddJoinRB(T: TREE3) : TREE4 =
(*red -> 0; black -> 1*)
  struct
    open T;

    datatype jInput = KVIn of (int * treeValueType) | KVPIn of (int * treeValueType * int);

    val Red   = 0;
    val Black = 1;

    fun singleton (k: int, v: treeValueType) : tree =
      makeTree(Empty, (k, v, Black), Empty)

    fun join (L: tree, kvIn: jInput, R: tree): tree =
      let
        val k = (case kvIn of
                      KVIn (k, _) => k
                    | KVPIn (k, _, _) => k)
        val v = (case kvIn of
                      KVIn (_, v) => v
                    | KVPIn (_, v, _) => v)

        fun eToKVC (e: elem) : (int * treeValueType * int) =
          (case e of
                (Container (k, v, c)) =>
                  (k, v, c)
              | (AVContainer ((k, v, c), _)) =>
                  (k, v, c))

        fun getColor (Empty) : int = Black
          | getColor (Node (_, Container(_, _, color), _)) = color
          | getColor (Node (_, AVContainer((_, _, color), _), _)) = color

        fun setColor ((Node (T1: tree, e1: elem, T2: tree)), color: int) : tree =
          let
            val e2 = (case e1 of
               (Container ((k, v, _))) => Container ((k, v, color))
             | (AVContainer ((k, v, _), augval)) => AVContainer ((k, v, color), augval))
          in
            Node (T1, e2, T2)
          end

        fun getRank(T1: tree) : int =
          let
            fun getBlackHeight(T2: tree) : int =
              (case T2 of
                   Empty => 0
                 | _ => if (getColor(T2) = Black) then 1 + getBlackHeight(getLChild(T2))
                        else getBlackHeight(getLChild(T2)))
          in
            if getColor(T1) = Black then 2 * (getBlackHeight(T1) - 1) 
            else (2 * getBlackHeight(T1)) - 1
          end

        fun joinRightRB(L1: tree, (k1: int, v1: treeValueType), R1: tree)
          : tree = 
          if (getRank (L1) = floor(Real.fromInt(getRank (R1))/2.0) * 2)
          then makeTree (L1, (k1, v1, Red), R1) 
          else
            let
              val L1' = (case L1 of
                  (Node (L', Container (k', v', c'), R')) =>
                    (L', k', v', c', R')
                | (Node (L', AVContainer ((k', v', c'), _), R')) =>
                    (L', k', v', c', R'))
              val T' = makeTree (#1(L1'), (#2(L1'), #3(L1'), #4(L1')),
              joinRightRB(#5(L1'), (k1, v1), R1))
            in
              if ((#4(L1') = Black) andalso (getColor(getRChild(T')) = Red
              andalso getColor(getRChild(getRChild(T'))) = Red)) then
                rotateLeft (makeTree (#1(L1'), (#2(L1'), #3(L1'), #4(L1')),
                makeTree (getLChild(getRChild(T')), eToKVC (getElem(getRChild(T'))), 
                setColor (getRChild(getRChild(T')), Black))))
              else
                T'
            end

        fun joinLeftRB(L1: tree, (k1: int, v1: treeValueType), R1: tree)
          : tree =
          if (getRank (R1) = floor(Real.fromInt(getRank (L1))/2.0) * 2)
          then makeTree (L1, (k1, v1, Red), R1)
          else
            let
              val R1' = (case R1 of
                  (Node (L', Container (k', v', c'), R')) =>
                    (L', k', v', c', R')
                | (Node (L', AVContainer ((k', v', c'), _), R')) =>
                    (L', k', v', c', R'))
              val T' = makeTree (joinLeftRB(L1, (k1, v1), #1(R1')),
              (#2(R1'), #3(R1'), #4(R1')), #5(R1'))
            in
              if ((#4(R1') = Black) andalso (getColor(getLChild(T')) = Red
              andalso getColor(getLChild(getLChild(T'))) = Red)) then
                rotateRight (makeTree (makeTree (setColor (getLChild(getLChild(T')), Black), 
                eToKVC (getElem(getLChild(T'))), getRChild(getLChild(T'))), 
                (#2(R1'), #3(R1'), #4(R1')), #5(R1')))
              else
                T'
            end
      in 
        if (floor(Real.fromInt(getRank(L))/2.0) >
        floor(Real.fromInt(getRank(R))/2.0)) then
          let
            val T' = joinRightRB(L, (k, v), R)
          in
            if ((getColor(T') = Red) andalso (getColor(getRChild(T')) = Red)) then
              let
                val kvc = eToKVC (getElem (T'))
              in
                makeTree (getLChild(T'), (#1(kvc), #2(kvc), Black), getRChild(T'))
              end
            else T'
          end

        else if (floor(Real.fromInt(getRank(R))/2.0) >
        floor(Real.fromInt(getRank(L))/2.0)) then
          let
            val T' = joinLeftRB(L, (k, v), R)
          in
            if ((getColor(T') = Red) andalso (getColor(getLChild(T')) = Red)) then
              let
                val kvc = eToKVC (getElem (T'))
              in
                makeTree (getLChild(T'), (#1(kvc), #2(kvc), Black), getRChild(T'))
              end
            else T'
          end
        else if ((getColor(L) = Black) andalso (getColor(R) = Black)) then
          makeTree (L, (k, v, Red), R)
          
        else makeTree (L, (k, v, Black), R)
      end
  end;

functor AddJoinAVL (T: TREE3): TREE4 =
  struct
    open T;

    datatype jInput = KVIn of (int * treeValueType) | KVPIn of (int * treeValueType * int);

    fun singleton (k, v): tree =
      makeTree(Empty, (k, v, 1), Empty)

    fun join (L: tree, kvIn: jInput, R: tree): tree =
      let
        val k = (case kvIn of
                      KVIn (k, _) => k
                    | KVPIn (k, _, _) => k)
        val v = (case kvIn of
                      KVIn (_, v) => v
                    | KVPIn (_, v, _) => v)

       fun getHeight(Empty) = 0
          | getHeight(Node(_, Container(_, _, h), _)) = h
          | getHeight(Node(_, AVContainer((_, _, h), _), _)) = h

        (*Recalculate heights and augmented values stored at affected nodes after a rotate*)
        fun rotateFixup(Empty) = Empty
          | rotateFixup(Node (L1, Container(k1, v1, _), R1)) =
          makeTree (L1, (k1, v1, Int.max(getHeight(L1), getHeight(R1)) + 1), R1)
          | rotateFixup(Node (L1, AVContainer((k1, v1, _), _), R1)) =
          makeTree (L1, (k1, v1, Int.max(getHeight(L1), getHeight(R1)) + 1), R1)

        fun rotateRightFixup(Node(L1, e1, R1)): tree =
          rotateFixup(Node (L1, e1, rotateFixup(R1)))
          
        fun rotateLeftFixup(Node(L1, e1, R1)): tree =
          rotateFixup(Node (rotateFixup(L1), e1, R1))
        (**)
 
        fun setHeight(Node (L1, Container(k', v', _), R1), h: int): tree =
          Node (L1, Container ((k', v', h)), R1)
          | setHeight(Node (L1, AVContainer((k', v', _), av'), R1), h: int): tree = 
          Node (L1, AVContainer ((k', v', h), av'), R1)

        fun joinRightAVL(L1: tree, (k: int, v: treeValueType), R1: tree): tree =
          let
            val L1' = (case L1 of
                            Node (L1L, Container(L1k, L1v, L1h), L1R) => 
                            (L1L, L1k, L1v, L1h, L1R)
                          | Node (L1L, AVContainer((L1k, L1v, L1h), _), L1R) =>
                            (L1L, L1k, L1v, L1h, L1R))
          in
            if (getHeight(#5(L1')) <= (getHeight(R1) + 1)) then
              let
                val T' = makeTree(#5(L1'), (k, v, (getHeight(R1) + 1)), R1)
              in
                if (getHeight(T') <= getHeight(#1(L1')) + 1) then
                  makeTree(#1(L1'), (#2(L1'), #3(L1'), getHeight(#1(L1')) + 1), T')
                else
                  let
                    val T'r = rotateRightFixup(rotateRight(T'))
                  in
                    rotateLeftFixup(rotateLeft(makeTree(#1(L1'), (#2(L1'), #3(L1'), 
                    (Int.max(getHeight(#1(L1')), getHeight(T'r)) + 1)), T'r)))
                  end
              end
            else
              let
                val T' = joinRightAVL(#5(L1'), (k, v), R1)
                val T'' = makeTree(#1(L1'), (#2(L1'), #3(L1'),
                (Int.max(getHeight(#1(L1')), getHeight(T')) + 1)), T')
              in
                if (getHeight(T') <= getHeight(#1(L1'))) then T''
                else rotateLeftFixup(rotateLeft(T''))
              end
          end

        fun joinLeftAVL(L1: tree, (k: int, v: treeValueType), R1: tree): tree =
          let
            val R1' = (case R1 of
                            Node (R1L, Container(R1k, R1v, R1h), R1R) => 
                            (R1L, R1k, R1v, R1h, R1R)
                          | Node (R1L, AVContainer((R1k, R1v, R1h), _), R1R) =>
                            (R1L, R1k, R1v, R1h, R1R))
          in
            if (getHeight(#1(R1')) <= (getHeight(L1) + 1)) then
              let
                val T' = makeTree(L1, (k, v, (getHeight(L1) + 1)), #1(R1'))
              in
                if (getHeight(T') <= getHeight(#5(R1')) + 1) then
                  makeTree(T', (#2(R1'), #3(R1'), getHeight(#5(R1')) + 1), #5(R1'))
                else
                  let
                    val T'l = rotateLeftFixup(rotateLeft(T'))
                  in
                    rotateRightFixup(rotateRight(makeTree(T'l, (#2(R1'), #3(R1'), 
                    (Int.max(getHeight(#5(R1')), getHeight(T'l)) + 1)), #5(R1'))))
                  end
              end
            else
              let
                val T' = joinLeftAVL(L1, (k, v), #1(R1'))
                val T'' = makeTree(T', (#2(R1'), #3(R1'),
                (Int.max(getHeight(#5(R1')), getHeight(T')) + 1)), #5(R1'))
              in
                if (getHeight(T') <= getHeight(#5(R1'))) then T''
                else rotateRightFixup(rotateRight(T''))
              end
          end
      in
        if (getHeight(L) > getHeight(R) + 1) then joinRightAVL(L, (k, v), R)
        else if (getHeight(R) > getHeight(L) + 1) then joinLeftAVL(L, (k, v), R)
        else makeTree(L, (k, v, Int.max(getHeight(L), getHeight(R)) + 1), R)
      end

  end;

functor AddJoinWB(T: TREE3): TREE4 = 
  struct
    open T;

    datatype jInput = KVIn of (int * treeValueType) | KVPIn of (int * treeValueType * int);

    val ALPHA = 0.29;
    val BETA = 1.0 - ALPHA;

    (* Some of the following code encodes our chosen method of calculating
    * weights. In this case it's just the total number of nodes in the tree. we
    * denote these functions with a "(**)" *)
    fun singleton (k: int, v: treeValueType): tree = (**)
      makeTree(Empty, (k, v, 1), Empty)

    fun join (L: tree, kvIn: jInput, R: tree): tree =
      let
        val k = (case kvIn of
                      KVIn (k, _) => k
                    | KVPIn (k, _, _) => k)
        val v = (case kvIn of
                      KVIn (_, v) => v
                    | KVPIn (_, v, _) => v)

        fun balance (i1: int, i2: int): bool =
          let
            val isum = i1 + i2
          in
            if (isum = 0) then true
            else if (Real.fromInt(i1) / Real.fromInt(isum) < ALPHA orelse
            Real.fromInt(i1) / Real.fromInt(isum) > BETA) then
              false
            else
              true
          end

        fun getWeight(Empty) = 0
          | getWeight(Node (_, Container(_, _, w), _)) = w
          | getWeight(Node (_, AVContainer((_, _, w), _), _)) = w

        fun combineWeights (T1: tree, T2: tree): int =
          getWeight(T1) + getWeight(T2)

        fun calcWeight(T1: tree): int = (**)
          let
            fun getTreeSize(Empty) = 0
              | getTreeSize(Node(T1L, _, T1R)) = 1 + getTreeSize(T1L) + getTreeSize(T1R)
          in
            getTreeSize(T1)
          end

        fun heavy (T1: tree, T2: tree): bool = getWeight(T1) > getWeight(T2)

        (*Recalculate heights and augmented values stored at affected nodes after a rotate*)
        fun rotateFixup(Empty) = Empty
          | rotateFixup(Node (L1, Container(k1, v1, _), R1)) =
          makeTree (L1, (k1, v1, getWeight(L1) + getWeight(R1) + 1), R1)
          | rotateFixup(Node (L1, AVContainer((k1, v1, _), _), R1)) =
          makeTree (L1, (k1, v1, getWeight(L1) + getWeight(R1) + 1), R1)

        fun rotateRightFixup(Node(L1, e1, R1)): tree =
          rotateFixup(Node (L1, e1, rotateFixup(R1)))

        fun rotateLeftFixup(Node(L1, e1, R1)): tree =
          rotateFixup(Node (rotateFixup(L1), e1, R1))
        (**)

        fun joinRightWB (L1: tree, (k1: int, v1: treeValueType), R1: tree): tree =
          if (balance(getWeight(L1), getWeight(R1))) then 
            makeTree(L1, (k1, v1, combineWeights(L1, R1) + 1), R1) (**)
          else
            let
              val x1 = (case L1 of
                            Node (L1L, Container(L1k, L1v, _), L1R) => 
                              (L1L, L1k, L1v, L1R)
                          | Node (L1L, AVContainer((L1k, L1v, _), _), L1R) =>
                              (L1L, L1k, L1v, L1R))
              val T' = joinRightWB (#4(x1), (k1, v1), R1)
              val x2 = (case T' of
                             Node (T'L, _, T'R) => (T'L, T'R))
            in
              if (balance(getWeight(#1(x1)), getWeight(T'))) then
                makeTree (#1(x1), (#2(x1), #3(x1), combineWeights(#1(x1), T') +
                1), T') (**)
              else if (balance (getWeight(#1(x1)), getWeight(#1(x2))) andalso
              balance(getWeight(#1(x1)) + getWeight(#1(x2)), getWeight(#2(x2))))
              then rotateLeftFixup(rotateLeft(makeTree(#1(x1), (#2(x1), #3(x1),
              combineWeights(#1(x1), T') + 1), T'))) (**)
              else rotateLeftFixup(rotateLeft(makeTree(#1(x1), (#2(x1), #3(x1),
              combineWeights(#1(x1), T') + 1), rotateRightFixup(rotateRight(T'))))) (**)
            end
 
        fun joinLeftWB (L1: tree, (k1: int, v1: treeValueType), R1: tree): tree =
          if (balance(getWeight(L1), getWeight(R1))) then
            makeTree(L1, (k1, v1, combineWeights(L1, R1) + 1), R1) (**)
          else
            let
              val x1 = (case R1 of
                            Node (R1L, Container(R1k, R1v, _), R1R) =>
                              (R1L, R1k, R1v, R1R)
                          | Node (R1L, AVContainer((R1k, R1v, _), _), R1R) =>
                              (R1L, R1k, R1v, R1R))
              val T' = joinLeftWB (L1, (k1, v1), #1(x1))
              val x2 = (case T' of
                             Node (T'L, _, T'R) => (T'L, T'R))
            in
              if (balance(getWeight(#4(x1)), getWeight(T'))) then
                makeTree (T', (#2(x1), #3(x1), combineWeights(T', #4(x1)) + 1), #4(x1)) (**)
              else if (balance (getWeight(#4(x1)), getWeight(#2(x2))) andalso
              balance(getWeight(#4(x1)) + getWeight(#2(x2)), getWeight(#1(x2))))
              then rotateRightFixup(rotateRight(makeTree(T', (#2(x1), #3(x1), 
              combineWeights(T', #4(x1)) + 1), #4(x1)))) (**)
              else
                rotateRightFixup(rotateRight(makeTree(rotateLeftFixup(rotateLeft(T')), 
                (#2(x1), #3(x1), combineWeights(T', #4(x1)) + 1), #4(x1)))) (**)
            end
      in
        if heavy(L, R) then joinRightWB(L, (k, v), R)
        else if heavy(R, L) then joinLeftWB(L, (k, v), R)
        else makeTree(L, (k, v, combineWeights(L, R) + 1), R) (**)
      end
  end;

functor AddJoinTreap(T: TREE3): TREE4 =
  struct
    open T;

    datatype jInput = KVIn of (int * treeValueType) | KVPIn of (int * treeValueType * int);

    val priorityRange: IntInf.int = 10000;

    fun getSeed(): int = 
      let
        val c = Time.toNanoseconds(Time.now()) div 1000
      in
        IntInf.toInt(c mod priorityRange)
      end

    fun getRandInt(): int=
      Random.randRange(0, IntInf.toInt(priorityRange)) (Random.rand(1, getSeed()))

    fun singleton(k: int, v: treeValueType) =
      let
        val r = Random.randRange (0, IntInf.toInt(priorityRange)) (Random.rand (1, getSeed()))
      in
        makeTree(Empty, (k, v, r), Empty)
      end

    fun singleton2(k: int, v: treeValueType, p: int) =
      makeTree(Empty, (k, v, p), Empty)

    fun join(L: tree, kvIn: jInput, R: tree) =
      let
        val k = (case kvIn of
                      KVIn (k, _) => k
                    | KVPIn (k, _, _) => k)
        val v = (case kvIn of
                      KVIn (_, v) => v
                    | KVPIn (_, v, _) => v)
        val p = (case kvIn of
                      KVIn (_, _) => getRandInt()
                    | KVPIn (_, _, p) => p)

        fun getKVP (T1: tree): (int * treeValueType * int) =
          (case T1 of
                Node (_, Container (k1, v1, p1), _) => (k1, v1, p1)
              | Node (_, AVContainer ((k1, v1, p1), _), _) => (k1, v1, p1))

        fun getPriority (Empty): int = (~1)
          | getPriority (Node(_, Container(_, _, p1), _)) = p1
          | getPriority (Node(_, AVContainer((_, _, p1), _), _)) = p1

        fun prior (i1: int, i2: int): bool = i1 > i2

        fun joinTreap (Empty, (k1: int, v1: treeValueType, p1: int), Empty): tree =
          singleton2 (k1, v1, p1)
          | joinTreap (Empty, (k1: int, v1: treeValueType, p1: int), R1): tree =
          if (prior(getPriority(R1), p1)) then
            let
              val T' = rotateLeft(makeTree(Empty, (k1, v1, p1), R1))
            in
              makeTree(joinTreap(getLChild(getLChild(T')), getKVP(getLChild(T')), getRChild(getLChild(T'))), 
              getKVP(T'), getRChild(T'))
            end
          else
            makeTree(Empty, (k1, v1, p1), R1)
          | joinTreap (L1, (k1: int, v1: treeValueType, p1: int), Empty): tree =
          if (prior(getPriority(L1), p1)) then
            let
              val T' = rotateRight(makeTree(L1, (k1, v1, p1), Empty))
            in
              makeTree(getLChild(T'), getKVP(T'),
              joinTreap(getLChild(getRChild(T')), getKVP(getRChild(T')),
              getRChild(getRChild(T'))))
            end
          else
            makeTree(L1, (k1, v1, p1), Empty)
          | joinTreap(L1, (k1: int, v1: treeValueType, p1: int), R1): tree = 
          if ((prior(p1, getPriority(L1))) andalso (prior(p1, getPriority(R1)))) then
            makeTree(L1, (k1, v1, p1), R1)
          else
            let
              val L1' = (case L1 of
                             Empty => (Empty, ~1, v1, ~1, Empty)
                           | Node (L1L, Container (L1k, L1v, L1p), L1R) =>
                             (L1L, L1k, L1v, L1p, L1R)
                           | Node (L1L, AVContainer ((L1k, L1v, L1p), _), L1R) =>
                             (L1L, L1k, L1v, L1p, L1R))
              val R1' = (case R1 of
                             Empty => (Empty, ~1, v1, ~1, Empty)
                           | Node (R1L, Container (R1k, R1v, R1p), R1R) =>
                             (R1L, R1k, R1v, R1p, R1R)
                           | Node (R1L, AVContainer ((R1k, R1v, R1p), _), R1R) =>
                             (R1L, R1k, R1v, R1p, R1R))
            in
              if (prior(#4(L1'), #4(R1'))) then
                makeTree(#1(L1'), (#2(L1'), #3(L1'), #4(L1')), joinTreap(#5(L1'),
                (k1, v1, p1), R1))
              else
                makeTree(joinTreap(L1, (k1, v1, p1), #1(R1')), (#2(R1'), #3(R1'),
                #4(R1')), #5(R1'))
            end
      in
        if ((prior(p, getPriority(L))) andalso (prior(p, getPriority(R)))) then
          makeTree(L, (k, v, p), R)
        else
          let
            val L' = (case L of
                           Empty => (Empty, ~1, v, ~1, Empty)
                         | Node (LL, Container (Lk, Lv, Lp), LR) => 
                           (LL, Lk, Lv, Lp, LR)
                         | Node (LL, AVContainer ((Lk, Lv, Lp), _), LR) =>
                           (LL, Lk, Lv, Lp, LR))
            val R' = (case R of
                           Empty => (Empty, ~1, v, ~1, Empty)
                         | Node (RL, Container (Rk, Rv, Rp), RR) => 
                           (RL, Rk, Rv, Rp, RR)
                         | Node (RL, AVContainer ((Rk, Rv, Rp), _), RR) =>
                           (RL, Rk, Rv, Rp, RR))
          in
            if (prior(#4(L'), #4(R'))) then
              makeTree(#1(L'), (#2(L'), #3(L'), #4(L')), joinTreap(#5(L'), (k, v, p), R))
            else
              makeTree(joinTreap(L, (k, v, p), #1(R')), (#2(R'), #3(R'), #4(R')), #5(R'))
          end
      end
  end;

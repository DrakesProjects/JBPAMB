open x3T;

val t1 = concatTree (Empty, (1, 2, 0), Empty);

val t2 = concatTree (t1, (2, 4, 1), t1);

val t3 = concatTree (t2, (5, 6, 0), t1);

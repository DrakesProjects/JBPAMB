# JBPAMB
Join-based parallel augmented map structure builder in Standard ML for testing different ways to allocate augmented values to nodes

1.	Broad Summary of program: Intended as a library for the easy and dynamic implementation of various join-based binary search tree balancing schemes in SML. Each node stores a key, a value, a balancing criterion, an augmented value (optional), and two subtrees. The initial purpose of this program is to test hypotheses relating to implementations of augmented values in join-based binary search trees.

2.	How it works: The program essentially works like an assembly line, tacking on various functionalities at each step through the use of functors, allowing for a great degree of customization in the creation of the data structure. The first step is to define a structure which aligns with the signature TREE as defined in the program tree.sml. After that, all that is needed is to plug the structure into a series of functors which automatically define the rest of the functions needed to form the data structure you intend on using.

3.	Stages of creating a tree:
   
      a.	tree.sml* (TREE):  this is the only structure          that requires you to actually define its functions.       Here you define the type of the values paired with        each key as well as the types for augmented values,       base and combine functions for the augmented              values, and the integer limit, which is used in our       implementation of augmented values in

      b.	tree1.sml (TREE  TREE1): Implements many              general functions used across all trees: getLChild,       getRChild, getElem, hasAV, getAV, concatTree,             concatTreeAV

  	      i.	MakeTree1

      c.	tree2.sml* (TREE1  TREE2): Here we choose how         we want our tree to decide when an augmented value        should be added to a node. Limit is used in               maxpathcheck and avfsizecheck.

  	      i.	AddMaxPathCheck- Size of max path to an                augmented value or leaf is less than limit
         ii.	AddAVFSizeCheck- Size of augmented value            frontier is less than limit
         iii.	AddAlwaysTrueCheck: Always add an                   augmented value
         iv.	AddAlwaysFalseCheck: Never add an                   augmented value

      d.	tree3.sml (TREE2  TREE3):  General functions          such as makeTree, and the rotates are added

  	      i.	AddRotates

      e.	tree4.sml* (TREE3  TREE4): Join is implemented        depending on the balancing criteria, RB, WB, Treap,       AVL

  	      i.	AddJoinRB: adds the join function for Red-             Black trees
         ii.	AddJoinAVL: adds the join function for AVL          trees
         iii.	AddJoinWB: adds the join method for
  	      weight-balanced trees
         iv.	AddJoinTreap: adds the join function for            treaps

         f.	tree5.sml (TREE4  TREE5): Finally, a series           of tests as well as top level functions such as           insert are implemented

  	        i.	AddFinalFunctions
  	
*Customization of the tree involved at this step


Future steps:
1.	Create a general function/functor which doesn’t require you to call all the functors every time you want to make a new data structure. Should ideally be 1 for all of them…
2.	Create an automated randomized  tree builder to test out the trees


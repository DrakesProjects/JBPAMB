structure x1     = MakeTree1 (RBTree);

(*
structure x2MP   = AddMaxPathCheck(x1);
structure x2AVF  = AddAVFSizeCheck(x1);
structure x2F    = AddAlwaysFalseCheck(x1);
*)
structure x2T    = AddAlwaysTrueCheck(x1);
structure x3T    = AddRotates(x2T);
structure x4T    = AddJoinRB(x3T);



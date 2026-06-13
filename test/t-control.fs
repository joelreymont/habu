\ t-control.fs — IF/loops/RECURSE via CHECK-DEF. ( -- ) words for clean catch.

\ balanced IF/ELSE/THEN
: C-IF  s" CIF" s" R i64 bool -- R i64" s" IF 1+ ELSE 1- THEN" CHECK-DEF ;
T{ ' C-IF catch -> 0 }T

\ IF with no ELSE: branch must be identity
: C-IF1 s" CIF1" s" R i64 bool -- R i64" s" IF 1+ THEN" CHECK-DEF ;
T{ ' C-IF1 catch -> 0 }T

\ imbalanced branches -> E-BRANCH
: C-BAD s" CBAD" s" R i64 bool -- R i64" s" IF 1+ ELSE DUP THEN" CHECK-DEF ;
T{ ' C-BAD catch -> E-BRANCH }T

\ IF with no-ELSE that changes the stack -> E-BRANCH
: C-BAD1 s" CBAD1" s" R i64 bool -- R i64" s" IF DUP THEN" CHECK-DEF ;
T{ ' C-BAD1 catch -> E-BRANCH }T

\ stable BEGIN/UNTIL
: C-BU  s" CBU" s" R i64 -- R i64" s" BEGIN 1- DUP 0= UNTIL" CHECK-DEF ;
T{ ' C-BU catch -> 0 }T

\ growing loop -> E-LOOP
: C-GROW s" CGROW" s" R i64 -- R i64" s" BEGIN 5 DUP 0= UNTIL" CHECK-DEF ;
T{ ' C-GROW catch -> E-LOOP }T

\ BEGIN/WHILE/REPEAT
: C-BWR s" CBWR" s" R i64 -- R i64" s" BEGIN DUP 0= WHILE 1- REPEAT" CHECK-DEF ;
T{ ' C-BWR catch -> 0 }T

\ ?DO/LOOP consuming ( limit index )
: C-DO  s" CDO" s" R i64 i64 -- R" s" ?DO LOOP" CHECK-DEF ;
T{ ' C-DO catch -> 0 }T

\ RECURSE uses the declared effect
: C-REC s" CREC" s" R i64 -- R i64" s" DUP 0= IF ELSE 1- RECURSE THEN" CHECK-DEF ;
T{ ' C-REC catch -> 0 }T

\ LEAVE: the stack at LEAVE must equal the loop-exit row (= the post-?DO row of
\ a stack-neutral body). A neutral LEAVE inside a branch is fine.
: C-LV  s" CLV" s" R i64 i64 -- R" s" ?DO I 7 = IF LEAVE THEN LOOP" CHECK-DEF ;
T{ ' C-LV catch -> 0 }T

\ LEAVE carrying an extra value -> the exit row no longer matches -> E-LOOP.
\ (Was silently certified when LEAVE had no effect; the inferred sig was a lie.)
: C-LVB s" CLVB" s" R i64 i64 -- R" s" ?DO 99 LEAVE LOOP" CHECK-DEF ;
T{ ' C-LVB catch -> E-LOOP }T

\ LEAVE outside any loop -> E-LOOP.
: C-LVN s" CLVN" s" R -- R" s" LEAVE" CHECK-DEF ;
T{ ' C-LVN catch -> E-LOOP }T

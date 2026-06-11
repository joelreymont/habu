\ t-selfhost.fs — self-host gate. Re-implement the checker's own type-term
\ encoding (from src/types.fs) as CHECKED definitions and run them through the
\ checker itself, with the host constants it reads annotated. This proves the
\ type system is expressive enough to describe code of its own kind, and that the
\ re-checked logic matches the native encoding.  (Loaded after the `:` override.)

\ Annotate the host constants the encoding reads (each pushes one i64). This is
\ the designed escape hatch — TRUSTED:/CHART for words whose effect we assert.
ARENA-RESET s" R -- R i64" PARSE-SIG s" T-CON"  CHART
ARENA-RESET s" R -- R i64" PARSE-SIG s" T-VAR"  CHART
ARENA-RESET s" R -- R i64" PARSE-SIG s" S-ROW"  CHART
ARENA-RESET s" R -- R i64" PARSE-SIG s" S-PUSH" CHART

\ The COMPLETE term-encoding layer of types.fs + rows.fs, re-checked through
\ habu's own `:` — both type-term and stack-term encodings.
: C-TAG    ( i64 -- i64 )   7 and ;
: C-PAY    ( i64 -- i64 )   3 rshift ;
: C-MKCON  ( i64 -- i64 )   3 lshift T-CON or ;
: C-MKVAR  ( i64 -- i64 )   3 lshift T-VAR or ;
: C-ISCON  ( i64 -- bool )  7 and T-CON = ;
: C-ISVAR  ( i64 -- bool )  7 and T-VAR = ;
: C-MKROW  ( i64 -- i64 )   3 lshift S-ROW or ;
: C-ISROW  ( i64 -- bool )  7 and S-ROW = ;
: C-ISPUSH ( i64 -- bool )  7 and S-PUSH = ;

\ all six compiled (each was checked): they must exist and match types.fs
T{ s" C-MKCON" find-name 0<> -> true }T
T{ 5 C-MKCON -> 5 MK-CON }T               \ same as the native encoding
T{ 5 C-MKCON C-TAG -> T-CON }T
T{ 5 C-MKCON C-PAY -> 5 }T
T{ 7 C-MKVAR C-PAY -> 7 }T
T{ 5 C-MKCON C-ISCON -> true }T
T{ 5 C-MKCON C-ISVAR -> false }T
T{ 9 C-MKVAR C-ISVAR -> true }T
T{ 3 C-MKROW -> 3 MK-ROW }T               \ stack-term encoding matches rows.fs
T{ 3 C-MKROW C-ISROW -> true }T
T{ 3 C-MKROW C-ISPUSH -> false }T

\ --- algorithmic core, re-checked through habu (host array access trusted) ---
ARENA-RESET s" R i64 -- R i64" PARSE-SIG s" TV@"      CHART   \ id -> bound term
ARENA-RESET s" R -- R i64"     PARSE-SIG s" UNBOUND"  CHART

\ the concrete-vs-concrete case of UNIFY-TYPE: two concretes unify iff same code
: C-UNICON ( i64 i64 -- bool )   C-PAY SWAP C-PAY = ;

\ one level of RESOLVE-TYPE: a bound var follows its binding, else stays put
\ (the real RESOLVE-TYPE wraps this in a BEGIN…AGAIN loop)
: C-RESOLVE1 ( i64 -- i64 )
   DUP C-ISVAR IF
      DUP C-PAY TV@  DUP UNBOUND = IF DROP ELSE NIP THEN
   THEN ;

\ these compiled (each was type-checked) and compute the real results
T{ s" C-UNICON"   find-name 0<> -> true }T
T{ s" C-RESOLVE1" find-name 0<> -> true }T
T{ TC-I64 MK-CON  TC-I64 MK-CON  C-UNICON -> true }T     \ i64 ~ i64
T{ TC-I64 MK-CON  TC-BOOL MK-CON C-UNICON -> false }T    \ i64 ≁ bool
\ resolve: bind var 0 -> i64, then C-RESOLVE1 of var 0 follows it
T{ TV-RESET TC-I64 MK-CON 0 TV!  0 MK-VAR C-RESOLVE1 -> TC-I64 MK-CON }T
T{ TC-BOOL MK-CON C-RESOLVE1 -> TC-BOOL MK-CON }T        \ concrete stays put

\ the FULL looping RESOLVE-TYPE, re-checked through habu (BEGIN/WHILE/REPEAT):
\ chase bindings until a concrete or unbound term.
: C-RESOLVE ( i64 -- i64 )
   BEGIN
     DUP C-ISVAR IF
        DUP C-PAY TV@  DUP UNBOUND = IF DROP false ELSE NIP true THEN
     ELSE false THEN
   WHILE REPEAT ;
T{ s" C-RESOLVE" find-name 0<> -> true }T
\ a 2-link chain  var0 -> var1 -> i64  resolves all the way through
T{ TV-CLEAR 1 MK-VAR 0 TV!  TC-I64 MK-CON 1 TV!  0 MK-VAR C-RESOLVE -> TC-I64 MK-CON }T
T{ TV-CLEAR 5 MK-VAR C-RESOLVE -> 5 MK-VAR }T           \ unbound var → itself

\ OCCURS (var/con cases): does type-var id occur in (resolved) t?  — the real
\ OCCURS-TYPE logic for the reachable cases (ptr/quot descent is the deferred part)
: C-OCCURS ( id t -- bool )
   C-RESOLVE  DUP C-ISVAR IF C-PAY = ELSE 2DROP false THEN ;
T{ s" C-OCCURS" find-name 0<> -> true }T
T{ TV-CLEAR  0  0 MK-VAR   C-OCCURS -> true }T          \ 0 occurs in var0
T{ TV-CLEAR  0  1 MK-VAR   C-OCCURS -> false }T         \ 0 not in var1
T{ TV-CLEAR  0  TC-I64 MK-CON C-OCCURS -> false }T      \ 0 not in a concrete

\ UNIFY-TYPE (con/var cases): resolve both, bind a var to the other, or compare
\ concretes. Returns success (the real unifier throws; here we return a flag to
\ stay in the checked value subset). Mutates TV-BIND via the trusted TV!.
ARENA-RESET s" R i64 i64 -- R" PARSE-SIG s" TV!" CHART
: C-UNIFY ( a b -- bool )
   C-RESOLVE SWAP C-RESOLVE SWAP
   OVER C-ISVAR IF  SWAP C-PAY TV!  true
   ELSE  DUP C-ISVAR IF  C-PAY TV!  true
         ELSE  C-UNICON  THEN
   THEN ;
T{ s" C-UNIFY" find-name 0<> -> true }T
T{ TC-I64 MK-CON  TC-I64 MK-CON  C-UNIFY -> true }T     \ i64 ~ i64
T{ TC-I64 MK-CON  TC-BOOL MK-CON C-UNIFY -> false }T    \ i64 ≁ bool
\ unify var0 with i64 → binds var0, and resolving var0 now gives i64
T{ TV-CLEAR  0 MK-VAR TC-I64 MK-CON C-UNIFY  0 MK-VAR C-RESOLVE -> true TC-I64 MK-CON }T

\ a deliberately-wrong re-implementation is REJECTED by the self-check
\ ( declares i64->i64 but leaves two ): proves the gate actually checks
: C-BADENC ( i64 -- i64 ) DUP ;
T{ s" C-BADENC" find-name 0<> -> false }T

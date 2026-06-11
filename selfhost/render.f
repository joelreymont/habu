\ render.fs — render the checker's inferred residual stack (DCUR) back to readable
\ type names. Type variables get canonical letters a,b,c… (assigned bottom-to-top),
\ int=n, flag=f. The "render" half of the native sigparse/checker. Needs checker.fs.
\ Standalone has no emit/+!; chars go through a 1-byte buffer + type.
create ECH 1 allot
: EMIT1 {: c :} c ECH c! ECH 1 type ;
create SEEN 64 cells allot   variable NLET
: SEEN-RESET 0 BEGIN dup cells SEEN + UNBOUND swap ! 1 + dup 63 > UNTIL drop ;
: LET-OF {: vp :}
   vp cells SEEN + @ UNBOUND = IF NLET @ vp cells SEEN + ! NLET @ 1 + NLET ! THEN
   vp cells SEEN + @ 97 + ;
: CON-CH {: p :} p 1 = IF 110 ELSE p 2 = IF 102 ELSE 99 THEN THEN ;   \ n / f / c
: REND-TYPE {: t :} t T-RES {: r :}
   r TAG T-VAR = IF r PAY LET-OF EMIT1 ELSE
   r TAG T-CON = IF r PAY CON-CH EMIT1 ELSE 63 EMIT1 THEN THEN ;
create RBUF 64 cells allot   variable RBN
: REND-COLLECT {: s :}  0 RBN !  s
   BEGIN R-RES dup TAG S-PUSH = WHILE          \ no locals inside the loop
     dup P>TYPE RBN @ cells RBUF + !  RBN @ 1 + RBN !
     P>REST
   REPEAT drop ;
\ RENDER ( -- ) : print DCUR's residual stack bottom-to-top, space-separated.
: RENDER  SEEN-RESET 0 NLET !  DCUR @ REND-COLLECT
   RBN @ BEGIN dup 0 > WHILE 1 - dup cells RBUF + @ REND-TYPE 32 EMIT1 REPEAT drop ;

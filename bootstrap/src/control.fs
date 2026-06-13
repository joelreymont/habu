\ control.fs — IF/ELSE/THEN, BEGIN-loops, DO-loops, RECURSE, EXIT.
\ Fills the CHECK-CONTROL hook. Drives the checker's body cursor (B-NEXT) and
\ sub-checks branch tokens with CHECK-WORD; manipulates DCUR/RCUR via schemes.

\ A token is a structural closer (ends a segment) — incl. end-of-body.
: CLOSER? ( a u -- f )
   dup 0= if 2drop true exit then
   2dup s" ELSE"  CI= if 2drop true exit then
   2dup s" THEN"  CI= if 2drop true exit then
   2dup s" UNTIL" CI= if 2drop true exit then
   2dup s" WHILE" CI= if 2drop true exit then
   2dup s" REPEAT"CI= if 2drop true exit then
   2dup s" AGAIN" CI= if 2drop true exit then
   2dup s" LOOP"  CI= if 2drop true exit then
   2dup s" +LOOP" CI= if 2drop true exit then
   2drop false ;

\ Check tokens until (and returning) the next closer token.
: CHECK-SEG ( -- da du )
   begin B-NEXT 2dup CLOSER? 0= while CHECK-WORD repeat ;

\ Unify two row pairs (data, return); rethrow as `code` on failure (E-BRANCH/E-LOOP).
variable U2A variable U2B variable U2C variable U2D
: (U2) ( -- )  U2A @ U2B @ UNIFY-ROW  U2C @ U2D @ UNIFY-ROW ;
: 2UNIFY-OR ( cd td cr tr code -- )
   {: code :}  U2D ! U2C ! U2B ! U2A !
   ['] (U2) catch if code throw then ;

: POP-BOOL ( -- )  s" R bool -- R" APPLY-SCHEME ;

\ Loop-nesting depth: I valid at depth>=1, J at depth>=2. Reset per definition.
\ DOSD/DOSR snapshot each enclosing DO's loop-exit rows (= the post-?DO rows, a
\ neutral body leaves them unchanged) so LEAVE can assert it carries that exact
\ stack to the exit — a non-neutral LEAVE would otherwise be silently certified.
variable DO-DEPTH
16 constant MAX-DO
create DOSD MAX-DO cells allot   create DOSR MAX-DO cells allot
:noname ( -- ) 0 DO-DEPTH ! ; is CHECK-RESET
: PUSH-I64 ( -- )  TC-I64 MK-CON PUSH-DTYPE ;
: DO-I ( -- )  DO-DEPTH @ 0= if E-LOOP throw then PUSH-I64 ;
: DO-J ( -- )  DO-DEPTH @ 2 < if E-LOOP throw then PUSH-I64 ;

\ --- IF / ELSE / THEN ---
: DO-IF ( -- )
   POP-BOOL
   DCUR @ RCUR @ {: sd sr :}              \ entry snapshot
   CHECK-SEG {: ea eu :}                  \ then-branch; closer = ELSE or THEN
   DCUR @ RCUR @ {: td tr :}              \ then-result
   ea eu s" ELSE" CI= if
      sd DCUR ! sr RCUR !                 \ reset for else
      CHECK-SEG 2drop                     \ else-branch (closer = THEN)
      DCUR @ td RCUR @ tr E-BRANCH 2UNIFY-OR
   else
      td sd tr sr E-BRANCH 2UNIFY-OR      \ no else: then must equal entry
      sd DCUR ! sr RCUR !
   then ;

\ --- BEGIN … (UNTIL | AGAIN | WHILE … REPEAT) ---
: DO-BEGIN ( -- )
   DCUR @ RCUR @ {: sd sr :}
   CHECK-SEG {: ea eu :}
   ea eu s" UNTIL" CI= if POP-BOOL DCUR @ sd RCUR @ sr E-LOOP 2UNIFY-OR exit then
   ea eu s" AGAIN" CI= if          DCUR @ sd RCUR @ sr E-LOOP 2UNIFY-OR exit then
   ea eu s" WHILE" CI= if
      POP-BOOL  CHECK-SEG 2drop             \ until REPEAT
      DCUR @ sd RCUR @ sr E-LOOP 2UNIFY-OR exit then
   E-LOOP throw ;

\ --- DO / ?DO … (LOOP | +LOOP) ---
: DO-DO ( -- )
   s" R i64 i64 -- R" APPLY-SCHEME           \ consume ( limit index )
   DO-DEPTH @ MAX-DO >= if E-LOOP throw then
   DCUR @ RCUR @ {: sd sr :}
   sd DO-DEPTH @ cells DOSD + !  sr DO-DEPTH @ cells DOSR + !
   1 DO-DEPTH +!  CHECK-SEG  -1 DO-DEPTH +!  {: ea eu :}
   ea eu s" +LOOP" CI= if s" R i64 -- R" APPLY-SCHEME then
   DCUR @ sd RCUR @ sr E-LOOP 2UNIFY-OR ;

\ LEAVE jumps to the loop exit: the stack here must equal the loop-exit row of
\ the innermost enclosing DO (frame DO-DEPTH-1). Loop control isn't on the typed
\ rows, so there's no further effect. (UNLOOP stays a pure no-op.)
: DO-LEAVE ( -- )
   DO-DEPTH @ 0= if E-LOOP throw then
   DO-DEPTH @ 1 - {: f :}
   DCUR @  f cells DOSD + @   RCUR @  f cells DOSR + @   E-LOOP 2UNIFY-OR ;

: DO-RECURSE ( -- )  CUR-SIG@ APPLY-SCHEME ;   \ fresh instantiation of own effect
: DO-EXIT ( -- )                               \ assert current = declared output
   DCUR @ DECL @ EFF>DOUT UNIFY-ROW
   RCUR @ DECL @ EFF>ROUT UNIFY-ROW ;

: (CHECK-CONTROL) ( a u -- f )
   2dup s" IF"      CI= if 2drop DO-IF      true exit then
   2dup s" BEGIN"   CI= if 2drop DO-BEGIN   true exit then
   2dup s" ?DO"     CI= if 2drop DO-DO      true exit then
   2dup s" DO"      CI= if 2drop DO-DO      true exit then
   2dup s" RECURSE" CI= if 2drop DO-RECURSE true exit then
   2dup s" EXIT"    CI= if 2drop DO-EXIT    true exit then
   2dup s" I"       CI= if 2drop DO-I       true exit then
   2dup s" J"       CI= if 2drop DO-J       true exit then
   2dup s" LEAVE"   CI= if 2drop DO-LEAVE   true exit then
   2dup s" UNLOOP"  CI= if 2drop            true exit then   \ no data effect
   2drop false ;
' (CHECK-CONTROL) is CHECK-CONTROL

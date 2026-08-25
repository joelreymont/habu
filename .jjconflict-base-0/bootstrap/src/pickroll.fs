\ pickroll.fs — fold a LITERAL-argument PICK / ROLL into a concrete stack shuffle
\ at check time, so the common `2 PICK` / `1 ROLL` forms are typeable. A dynamic
\ (runtime-computed) index stays untypeable and falls through to E-UNCHECKED.
\
\   N PICK  copies the N-deep item to the top:  N=0 DUP, 1 OVER, 2 → ( a b c -- a b c a )
\   N ROLL  rotates the N-deep item to the top:  N=0 noop, 1 SWAP, 2 ROT
\
\ The literal N already pushed an i64 (the index) onto the abstract stack, so the
\ synthesized effect consumes that trailing `i64`. We build the signature string
\ with N+1 fresh type vars and APPLY-SCHEME it (re-using PARSE-SIG + unify).

26 constant MAX-PR                       \ N+1 distinct type-var letters (a..z)
create PRBUF 256 chars allot   variable PRLEN
: PR0  ( -- )      0 PRLEN ! ;
: PR+  ( a u -- )  PRBUF PRLEN @ +  swap dup PRLEN +!  move ;
: PR-LET ( i -- )  [char] a +  PRBUF PRLEN @ + c!  1 PRLEN +! ;     \ append one var letter
: PR-VAR ( i -- )  PR-LET  bl PRBUF PRLEN @ + c!  1 PRLEN +! ;      \ … plus a space

\ inputs: R v0 v1 … vN i64   (v0 deepest)
: PR-IN ( N -- )  PR0  s" R " PR+  1+ 0 ?do i PR-VAR loop  s" i64 -- R " PR+ ;

: PICK-SIG ( N -- a u )                   \ R v0..vN i64 -- R v0..vN v0
   dup PR-IN  dup 1+ 0 ?do i PR-VAR loop  drop  0 PR-LET  PRBUF PRLEN @ ;
: ROLL-SIG ( N -- a u )                   \ R v0..vN i64 -- R v1..vN v0
   dup PR-IN  dup 1+ 1 ?do i PR-VAR loop  drop  0 PR-LET  PRBUF PRLEN @ ;

: ?FOLD ( N xt -- f )                     \ apply sig from xt(N) if N in range
   over 0 MAX-PR within 0= if 2drop false exit then
   execute APPLY-SCHEME true ;

: (CHECK-PICK) ( a u -- f )
   CUR-PREV? @ 0= if 2drop false exit then              \ no literal before -> not foldable
   2dup s" PICK" CI= if 2drop CUR-PREV-VAL @ ['] PICK-SIG ?FOLD exit then
   2dup s" ROLL" CI= if 2drop CUR-PREV-VAL @ ['] ROLL-SIG ?FOLD exit then
   2drop false ;
' (CHECK-PICK) is CHECK-PICK

\ cglocals.fs — compile-time locals for the AOT codegen, mirroring the checker's
\ `{: a b :}` parsing. At the opener: spill the VS and pop the named inputs from
\ the data stack into a per-word locals FRAME ([sp,#slot*8], carved by g-prologue's
\ LOCSZ). A later use of a name LDRs its slot onto the VS. Inputs-only (v0), like
\ the checker. Names with a `:type` suffix are stored by bare name (NAME-PART).

require regstack.fs

CHECKING-ON? @  CHECKING-ON? off          \ metaprogramming (loops, name table)

32 constant CGL-MAX   64 constant CGL-NAMESZ
create CGL-NAME CGL-MAX CGL-NAMESZ chars * allot
create CGL-LEN  CGL-MAX cells allot
variable CGL-N        variable CGL-COLLECT?

: CGL-RESET ( -- )  0 CGL-N !  CGL-COLLECT? off ;

: CGL-NAME@ ( i -- a u )  dup CGL-NAMESZ chars * CGL-NAME +  swap cells CGL-LEN + @ ;

: CGL-ADD ( a u -- )                       \ record a local name, assign the next slot
   CGL-N @ CGL-MAX >= if 1 abort" cg: too many locals" then
   dup CGL-NAMESZ > if 1 abort" cg: local name too long" then
   CGL-N @ {: i :}
   dup i cells CGL-LEN + !
   i CGL-NAMESZ chars * CGL-NAME +  swap move
   1 CGL-N +! ;

: CGL-LOAD ( slot -- )  {: slot :}  R-ALLOC {: r :}  r SP slot 8 * LDR,  r V-PUSHR ;

: CGL-EMIT-POPS ( -- )                     \ pop declared locals (top = last) into slots
   CGL-N @ 0 ?do
      CGL-N @ 1- i -  {: slot :}
      V-POPR {: r :}  r SP slot 8 * STR,  r R-FREE
   loop ;

: CGL-FIND ( a u -- f )                    \ names a local? -> emit a load + true; else false
   CGL-N @ 0 ?do
      2dup i CGL-NAME@ CI= if  2drop  i CGL-LOAD  true  unloop exit  then
   loop  2drop false ;

\ Token hook for walk.fs EMIT-TOKEN (consumes a copy, returns handled?).
: CHECK-LOCAL-CG ( a u -- f )
   CGL-COLLECT? @ if
      2dup BRACE-CLOSE? if 2drop  CGL-EMIT-POPS  CGL-COLLECT? off  true  exit then
      2dup s" --" CI= if 2drop true exit then        \ inputs-only: skip a -- separator
      NAME-PART CGL-ADD  true  exit
   then
   2dup BRACE-OPEN? if 2drop  V-SPILL  0 CGL-N !  CGL-COLLECT? on  true  exit then
   CGL-FIND ;

CHECKING-ON? !                            \ restore

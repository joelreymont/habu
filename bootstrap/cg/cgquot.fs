\ cgquot.fs — AOT inlining of LITERAL quotations + combinators. `[: … ;]` is
\ captured (its tokens collected, not emitted); a following EXECUTE/DIP inlines
\ that body's ops directly (no `execute`, no indirect call). DIP saves the top on
\ the return stack, inlines the quotation under it, then restores it. Non-literal
\ xts and other combinators fall through (the word stays uncompilable → threaded).

require cglocals.fs

CHECKING-ON? @  CHECKING-ON? off          \ metaprogramming (span capture, recursion)

defer WALK-INLINE ( a u -- )              \ = WALK-BODY, bound in walk.fs (forward ref)

create QBUF 1024 chars allot   variable QLEN   variable QCAP?   variable QDEPTH
2variable QPEND                           \ captured quotation body (a u into QBUF), or 0 0

: q-reset ( -- )  QCAP? off  0 0 QPEND 2! ;

: q+ ( a u -- )  QBUF QLEN @ + swap dup QLEN +! move  bl QBUF QLEN @ + c!  1 QLEN +! ;

\ inline the pending quotation onto the current (already-spilled) stack
: q-exec ( -- )  v-spill  QPEND 2@ WALK-INLINE  q-reset ;

\ DIP: save TOS on the return stack, inline under it, restore
: q-dip ( -- )
   v-spill  T0 g-pop  T0 g-rpush          \ x -> return stack; rest stays in memory
   QPEND 2@ WALK-INLINE                    \ inline quot on the rest (ends with v-spill)
   T0 g-rpop  T0 g-push                    \ x back on top
   q-reset ;

: CHECK-QUOT-CG ( a u -- f )
   QCAP? @ if                              \ collecting a quotation body
      2dup s" [:" CI= if  1 QDEPTH +!  q+  true exit then
      2dup s" ;]" CI= if  -1 QDEPTH +!  QDEPTH @ 0= if
            2drop  QBUF QLEN @ QPEND 2!  QCAP? off  true exit
         then  q+  true exit then
      q+  true exit
   then
   2dup s" [:" CI= if  2drop  0 QLEN !  1 QDEPTH !  QCAP? on  true exit then
   QPEND 2@ nip if                         \ a literal quotation is pending → combinators
      2dup s" EXECUTE" CI= if  2drop  q-exec  true exit then
      2dup s" DIP"     CI= if  2drop  q-dip   true exit then
   then
   2drop false ;

CHECKING-ON? !

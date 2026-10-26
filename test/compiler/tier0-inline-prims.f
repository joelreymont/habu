\ tier0-inline-prims.f - tier 0 COPIES a straight-line engine primitive's body
\ into the word that names it instead of calling it.
\
\ The decision is made at compile time on the primitive's final bytes
\ (src/habu/habu2.f EMIT-CINLINE): a body with no call, return, PC-relative
\ address or literal load, and no branch leaving the body, is copied; any other
\ body keeps its BL. This file pins both halves through the real load path
\ (ARM's default tier is 0, so no tier is selected here): the inlined subjects
\ hold no BL into their primitive's span, the refused ones hold exactly one,
\ and every subject still answers what the call answered.
\
\ Spans come from the DICTIONARY RECORD (src/habu/xref.f), the same record the
\ compiler resolves a call against. The decoder's positive control is an
\ ordinary call between two definitions of this file: "no BL" is worth nothing
\ if the decoder cannot see a call at all.
\
\ A word whose body only inlines stays a leaf: EM-COMPILE-RET rewrites its entry
\ slot to a nop, so the entry word is asserted as well as the call census.

require src/lib/test.f
require src/lib/errors.f
require src/habu/xref.f
require src/habu/code-bytes.f
require src/compiler/native/branch.f

package TIER0-INLINE-PRIMS
private

$D503201F constant ENTRY-NOP         \ nop: the entry slot of a leaf
$F81F0FFE constant ENTRY-SAVE        \ str x30,[sp,#-16]!: the entry slot of a caller
$FFFFFFE0 constant REG-MASK          \ clears an instruction's Rt field
$F85F8E60 constant STACK-POP         \ ldr xR,[x19,#-8]!
$F8008660 constant STACK-PUSH        \ str xR,[x19],#8
$FC008660 constant FLOAT-PUSH        \ str dR,[x19],#8

\ CODE-BYTES:AT is the bounded view between an engine code address and a
\ readable pointer; this file adds none of its own.
: CODE@ ( n -- n )
   4 CODE-BYTES:AT drop {: p:ptr :}
   p c@
   p 1+ c@ 8 lshift or
   p 2 + c@ 16 lshift or
   p 3 + c@ 24 lshift or ;

variable MISSING                     \ names the dictionary did not answer for

: SPAN ( ptr u8 n -- n n ) {: na:ptr nu:n :}
   na nu XREF-FIND {: rec:ptr :}
   rec XREF-FOUND? 0= if
      1 MISSING +!
      s" tier0-inline-prims: no record for " type na nu type cr
      0 0 exit
   then
   rec XREF-START dup rec XREF-CODE-BYTES + ;

\ How many calls the named word makes into the named target's span.
: CALLS-WORD ( ptr u8 n ptr u8 n -- n ) {: na:ptr nu:n ta:ptr tu:n :}
   ta tu SPAN {: tlo:n thi:n :}
   na nu SPAN {: lo:n hi:n :}
   0
   hi lo ?do
      i CODE@ {: w:n :}
      w NBR:BL? if
         i w NBR:BL-TARGET {: t:n :}
         t tlo >= t thi < and if 1+ then
      then
   4 +loop ;

\ How many of the named word's instructions are the data-stack transfer op.
: STACK-OPS ( ptr u8 n n -- n ) {: na:ptr nu:n op:n :}
   na nu SPAN {: lo:n hi:n :}
   0
   hi lo ?do
      i CODE@ REG-MASK and op = if 1+ then
   4 +loop ;

: ENTRY@ ( ptr u8 n -- n )
   SPAN drop CODE@ ;

: INLINED ( ptr u8 n ptr u8 n -- ) CALLS-WORD 0 T= ;
: CALLED ( ptr u8 n ptr u8 n -- ) CALLS-WORD 1 T= ;

create T0-CELL 2 cells allot
create T0-TEXT 4 allot

public

\ ---- the subjects ------------------------------------------------------------
: T0-ADD ( n n -- n ) + ;
: T0-ROT ( n n n -- n n n ) rot ;
: T0-FETCH ( ptr n -- n ) @ ;
: T0-CFETCH ( ptr u8 -- n ) c@ ;
: T0-CELLS ( n -- n ) cells ;
: T0-ABS ( n -- n ) abs ;
: T0-MIN ( n n -- n ) min ;
: T0-MAX ( n n -- n ) max ;
: T0-2R ( n n -- n n ) 2>r 2r> ;
: T0-CHARS ( n -- n ) chars ;
: T0-COUNT ( ptr u8 -- ptr u8 n ) count ;
\ A frame-bearing body (sp lowered, x30 saved, restored, sp raised), copied
\ into a leaf whose entry slot became a nop.
: T0-ACA ( -- n ) addr-cells-abi ;

: T0-STORE ( n ptr n -- ) ! ;
: T0-DIV ( n n -- n ) / ;
: T0-TYPE ( ptr u8 n -- ) type ;
\ One subject per other refusal, each body refused by that arm alone: an ADR,
\ a BR, a branch leaving the body.  T0-CREATE is compiled for its shape only
\ and never run.
: T0-PRODUCER ( -- ptr u8 n bool ) kernel-producer$ ;
: T0-CREATE ( -- ) create ;
: T0-ALIGN ( -- ) align ;

\ The decoder's positive control: an ordinary call, not in tail position.
: T0-CALLEE ( n -- n ) 1 lshift ;
: T0-CALLER ( n -- n ) T0-CALLEE 1+ ;

\ An inlined body inside a loop and inside a quotation.
: T0-SPIN ( n n n n -- n n n ) {: a:n b:n c:n k:n :}
   a b c k begin >r rot r> 1- dup 0= until drop ;
: T0-QROT ( n n n -- n n n ) [: rot ;] execute ;

\ Arithmetic on cells the caller left on the data stack, one subject per entry
\ that pulls: binary, binary with an immediate, unary, shuffle, float.
: T0-SUB2 ( n n -- n ) - 3 * 1+ ;
: T0-IMM ( n -- n ) 4 - 3 * ;
: T0-UN ( n -- n ) 1+ 1+ ;
: T0-SHUF ( n n -- n ) swap - ;
: T0-FLT ( r r -- r ) f- 0.5 f+ ;

private

: TEST-DECODER ( -- )
   s" the call decoder finds an ordinary call between two definitions" T-LABEL
   s" TIER0-INLINE-PRIMS:T0-CALLER" s" TIER0-INLINE-PRIMS:T0-CALLEE" CALLED ;

: TEST-INLINED ( -- )
   s" a straight-line primitive is copied, not called" T-LABEL
   s" TIER0-INLINE-PRIMS:T0-ADD" s" +" INLINED
   s" TIER0-INLINE-PRIMS:T0-ROT" s" rot" INLINED
   s" TIER0-INLINE-PRIMS:T0-FETCH" s" @" INLINED
   s" TIER0-INLINE-PRIMS:T0-CFETCH" s" c@" INLINED
   s" TIER0-INLINE-PRIMS:T0-CELLS" s" cells" INLINED
   s" TIER0-INLINE-PRIMS:T0-ABS" s" abs" INLINED
   s" TIER0-INLINE-PRIMS:T0-MIN" s" min" INLINED
   s" TIER0-INLINE-PRIMS:T0-MAX" s" max" INLINED
   s" TIER0-INLINE-PRIMS:T0-2R" s" 2>r" INLINED
   s" TIER0-INLINE-PRIMS:T0-2R" s" 2r>" INLINED
   s" TIER0-INLINE-PRIMS:T0-CHARS" s" chars" INLINED
   s" TIER0-INLINE-PRIMS:T0-COUNT" s" count" INLINED
   s" TIER0-INLINE-PRIMS:T0-SPIN" s" rot" INLINED
   s" TIER0-INLINE-PRIMS:T0-ACA" s" addr-cells-abi" INLINED
   s" an empty primitive body leaves only the entry slot and the return" T-LABEL
   s" TIER0-INLINE-PRIMS:T0-CHARS" SPAN swap - 8 T= ;

: TEST-CALLED ( -- )
   s" a primitive with a call, an ADR, a BR or an outward branch is still called" T-LABEL
   s" TIER0-INLINE-PRIMS:T0-STORE" s" !" CALLED
   s" TIER0-INLINE-PRIMS:T0-DIV" s" /" CALLED
   s" TIER0-INLINE-PRIMS:T0-TYPE" s" type" CALLED
   s" TIER0-INLINE-PRIMS:T0-PRODUCER" s" kernel-producer$" CALLED
   s" TIER0-INLINE-PRIMS:T0-CREATE" s" create" CALLED
   s" TIER0-INLINE-PRIMS:T0-ALIGN" s" align" CALLED ;

: TEST-LEAF ( -- )
   s" a body that only inlines stays a leaf; one that calls keeps its frame" T-LABEL
   s" TIER0-INLINE-PRIMS:T0-ROT" ENTRY@ ENTRY-NOP T=
   s" TIER0-INLINE-PRIMS:T0-ACA" ENTRY@ ENTRY-NOP T=
   s" TIER0-INLINE-PRIMS:T0-STORE" ENTRY@ ENTRY-SAVE T= ;

: TEST-PULL ( -- )
   s" stack operands are popped into registers once and the result pushed once" T-LABEL
   s" TIER0-INLINE-PRIMS:T0-SUB2" STACK-POP STACK-OPS 2 T=
   s" TIER0-INLINE-PRIMS:T0-SUB2" STACK-PUSH STACK-OPS 1 T=
   s" TIER0-INLINE-PRIMS:T0-IMM" STACK-POP STACK-OPS 1 T=
   s" TIER0-INLINE-PRIMS:T0-IMM" STACK-PUSH STACK-OPS 1 T=
   s" TIER0-INLINE-PRIMS:T0-UN" STACK-POP STACK-OPS 1 T=
   s" TIER0-INLINE-PRIMS:T0-UN" STACK-PUSH STACK-OPS 1 T=
   s" TIER0-INLINE-PRIMS:T0-SHUF" STACK-POP STACK-OPS 2 T=
   s" TIER0-INLINE-PRIMS:T0-SHUF" STACK-PUSH STACK-OPS 1 T=
   \ the float operands are pulled as bits and the result pushed from a d-reg
   s" TIER0-INLINE-PRIMS:T0-FLT" STACK-POP STACK-OPS 2 T=
   s" TIER0-INLINE-PRIMS:T0-FLT" STACK-PUSH STACK-OPS 0 T=
   s" TIER0-INLINE-PRIMS:T0-FLT" FLOAT-PUSH STACK-OPS 1 T=
   10 4 T0-SUB2 19 T= ;

: TEST-VALUES ( -- )
   s" the copied bodies answer what the calls answered" T-LABEL
   3 4 T0-ADD 7 T=
   1 2 3 T0-ROT {: x:n y:n z:n :}
   x 2 T=  y 3 T=  z 1 T=
   $55AA T0-CELL !
   T0-CELL T0-FETCH $55AA T=
   T0-CELL byte-view T0-CFETCH $AA T=
   3 T0-CELLS 24 T=
   -5 T0-ABS 5 T=
   5 T0-ABS 5 T=
   3 9 T0-MIN 3 T=
   9 3 T0-MIN 3 T=
   3 9 T0-MAX 9 T=
   9 3 T0-MAX 9 T=
   5 7 T0-2R {: p:n q:n :}
   p 5 T=  q 7 T=
   5 T0-CHARS 5 T=
   3 T0-TEXT c!  T0-TEXT T0-COUNT {: n:n :} drop
   n 3 T=
   T0-ACA addr-cells-abi T=
   s" an inlined body in a loop and in a quotation" T-LABEL
   1 2 3 4 T0-SPIN {: a:n b:n c:n :}
   a 2 T=  b 3 T=  c 1 T=
   1 2 3 T0-QROT {: d:n e:n f:n :}
   d 2 T=  e 3 T=  f 1 T=
   s" the still-called bodies answer and refuse as before" T-LABEL
   9 T0-CELL T0-STORE
   T0-CELL @ 9 T=
   17 5 T0-DIV 3 T=
   [: 1 0 T0-DIV drop ;] E-DIV-ZERO TTHROWSQ ;

\ `?dup` is absent from checked bodies (docs/forth-card.md), so its subject is
\ compiled last, with checking suspended for the rest of this file.
0 set-check

public

: T0-QDUP ?dup ;

private

: TEST-QDUP ( -- )
   s" ?dup is copied, not called, and answers both ways" T-LABEL
   s" TIER0-INLINE-PRIMS:T0-QDUP" s" ?dup" INLINED
   depth {: base :}
   0 T0-QDUP depth base - 1 T= 0 T=
   7 T0-QDUP depth base - 2 T= 7 T= 7 T= ;

public

: RUN ( -- )
   T-RESET
   0 MISSING !
   TEST-DECODER
   TEST-INLINED
   TEST-CALLED
   TEST-LEAF
   TEST-PULL
   TEST-VALUES
   TEST-QDUP
   MISSING @ 0 T=
   T-REPORT ;

;package

TIER0-INLINE-PRIMS:RUN

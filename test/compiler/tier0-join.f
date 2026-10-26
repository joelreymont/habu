\ tier0-join.f - tier 0 reconciles a loop's virtual stack to its BEGIN layout
\ with a parallel copy.
\
\ `begin` records which register holds each virtual stack entry; the back edge
\ (src/habu/jit.f LVRECON) moves each entry into its recorded register, breaking
\ a cycle through x16, pushes the entries deeper than the record and pops the
\ recorded ones the body left in memory. This file pins it through the real load
\ path (ARM's default tier is 0) by counting instructions inside each loop, from
\ the back branch's target to the back branch: a body that keeps the layout
\ emits nothing there, a body that swaps two registers is one cycle through x16
\ and no memory traffic, and a body that pulls one cell below the record pushes
\ only that cell. Every subject also runs and prints its answer.
\
\ Spans come from the DICTIONARY RECORD (src/habu/xref.f), as in
\ test/compiler/tier0-compare-branch.f.

require src/lib/test.f
require src/lib/errors.f
require src/habu/xref.f
require src/habu/code-bytes.f

package TIER0-JOIN
private

$FFFFFFE0 constant POP-MASK     $F85F8E60 constant POP-OP      \ ldr xR,[x19,#-8]!
$FFFFFFE0 constant PUSH-MASK    $F8008660 constant PUSH-OP     \ str xR,[x19],#8
$FFE0FFFF constant MOV16-MASK   $AA0003F0 constant MOV16-OP    \ mov x16,xN
$FFFFFFE0 constant FROM16-MASK  $AA1003E0 constant FROM16-OP   \ mov xN,x16
$FF000010 constant BCOND-MASK   $54000000 constant BCOND-OP    \ b.cond
$FF000000 constant CBZ-MASK     $B4000000 constant CBZ-OP      \ cbz xR

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
      s" tier0-join: no record for " type na nu type cr
      0 0 exit
   then
   rec XREF-START dup rec XREF-CODE-BYTES + ;

\ The byte offset a b.cond or cbz at word w branches by: imm19 words, signed.
: BRANCH-OFF ( n -- n )
   5 rshift $7FFFF and {: imm:n :}
   imm $40000 and 0<> if imm $80000 - else imm then 4 * ;

: BRANCH? ( n -- bool ) {: w:n :}
   w BCOND-MASK and BCOND-OP =  w CBZ-MASK and CBZ-OP =  or ;

\ The loop of the named word: from its first backward conditional branch's
\ target up to that branch. A word without one has an empty loop and counts
\ nothing, which every subject's expectations refuse.
: LOOP-SPAN ( ptr u8 n -- n n ) {: na:ptr nu:n :}
   na nu SPAN {: lo:n hi:n :}
   0 0
   hi lo ?do
      i CODE@ {: w:n :}
      w BRANCH? if
         w BRANCH-OFF 0< if
            2drop  i w BRANCH-OFF +  i  leave
         then
      then
   4 +loop ;

\ How many instructions inside the named word's loop match op under mask.
: LOOP-OPS ( ptr u8 n n n -- n ) {: na:ptr nu:n mask:n op:n :}
   na nu LOOP-SPAN {: lo:n hi:n :}
   0
   hi lo ?do
      i CODE@ mask and op = if 1+ then
   4 +loop ;

: LOOP-LEN ( ptr u8 n -- n )  LOOP-SPAN swap - ;
: POPS ( ptr u8 n -- n )  POP-MASK POP-OP LOOP-OPS ;
: PUSHES ( ptr u8 n -- n )  PUSH-MASK PUSH-OP LOOP-OPS ;
: MOV16S ( ptr u8 n -- n )  MOV16-MASK MOV16-OP LOOP-OPS ;
: FROM16S ( ptr u8 n -- n )  FROM16-MASK FROM16-OP LOOP-OPS ;

public

\ ---- the subjects ------------------------------------------------------------
\ ( acc n ) both live in registers at `begin` and come back in them.
: R-EXACT ( n -- n ) 0 swap begin swap 1+ swap 1- dup 0= until drop ;
\ The two registers of `begin` come back exchanged: a 2-cycle.
: R-SWAP ( n -- n n ) dup 1+ begin swap 1- dup 0= until ;
\ `begin` records the accumulator alone; the body pulls the count below it.
: R-PULL ( n -- n ) 0 begin swap 1- swap 1+ over 0= until nip ;

private

: TEST-SHAPE ( -- )
   s" a back edge moves only the entries whose register differs" T-LABEL
   s" TIER0-JOIN:R-EXACT" LOOP-LEN 0 T<>
   s" TIER0-JOIN:R-EXACT" POPS 0 T=
   s" TIER0-JOIN:R-EXACT" PUSHES 0 T=
   s" TIER0-JOIN:R-EXACT" MOV16S 0 T=
   s" TIER0-JOIN:R-SWAP" LOOP-LEN 0 T<>
   s" TIER0-JOIN:R-SWAP" POPS 0 T=
   s" TIER0-JOIN:R-SWAP" PUSHES 0 T=
   s" TIER0-JOIN:R-SWAP" MOV16S 1 T=
   s" TIER0-JOIN:R-SWAP" FROM16S 1 T=
   s" TIER0-JOIN:R-PULL" LOOP-LEN 0 T<>
   s" TIER0-JOIN:R-PULL" POPS 1 T=
   s" TIER0-JOIN:R-PULL" PUSHES 1 T=
   s" TIER0-JOIN:R-PULL" MOV16S 0 T= ;

: TEST-VALUES ( -- )
   s" every subject answers what its loop computes" T-LABEL
   3 R-EXACT  dup . cr  3 T=
   3 R-SWAP  over . dup . cr  0 T=  2 T=
   3 R-PULL  dup . cr  3 T= ;

public

: RUN ( -- )
   T-RESET
   0 MISSING !
   TEST-SHAPE
   TEST-VALUES
   MISSING @ 0 T=
   T-REPORT ;

;package

TIER0-JOIN:RUN

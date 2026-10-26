\ tier0-join.f - tier 0 reconciles the virtual stack at every join to the
\ layout its opener recorded, with a parallel copy.
\
\ `begin`, `if` and `while` record which register holds each virtual stack
\ entry; the back edge, `then` and `repeat`'s exit (src/habu/jit.f LVRECON,
\ LVSETF) move each entry into its recorded register, breaking a cycle through
\ x16, push the entries deeper than the record and pop the recorded ones the
\ path left in memory, and `else` records the true arm's end for both arms
\ (LVRESNAP). This file pins it through the real load path (ARM's default
\ tier is 0) by counting instructions. A loop's count runs from the back
\ branch's target to the back branch: a body that keeps the layout emits
\ nothing there, a body that swaps two registers is one cycle through x16 and
\ no memory traffic, and a body that pulls one cell below the record pushes
\ only that cell. A word with joins counts its whole code: its inputs are
\ popped once and its outputs pushed once at `;`, so a join that spilled
\ would add both. A compare right before a branch fuses into a b.cond
\ (src/habu/jit.f LVFUSE) unless a join lands after the compare: J-FENCE's
\ `then` patches the first `if` to land past the compare's cset, so its second
\ `if` keeps the flag, while J-FUSED's `then` lands on the compare itself and
\ both of its `if`s fuse. Every subject also runs and its answer is checked.
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

\ How many instructions in lo..hi match op under mask.
: SPAN-OPS ( n n n n -- n ) {: lo:n hi:n mask:n op:n :}
   0
   hi lo ?do
      i CODE@ mask and op = if 1+ then
   4 +loop ;

\ How many instructions inside the named word's loop match op under mask.
: LOOP-OPS ( ptr u8 n n n -- n ) {: na:ptr nu:n mask:n op:n :}
   na nu LOOP-SPAN mask op SPAN-OPS ;

\ How many instructions in the named word's whole code match op under mask.
: WORD-OPS ( ptr u8 n n n -- n ) {: na:ptr nu:n mask:n op:n :}
   na nu SPAN mask op SPAN-OPS ;

: LOOP-LEN ( ptr u8 n -- n )  LOOP-SPAN swap - ;
: POPS ( ptr u8 n -- n )  POP-MASK POP-OP LOOP-OPS ;
: PUSHES ( ptr u8 n -- n )  PUSH-MASK PUSH-OP LOOP-OPS ;
: MOV16S ( ptr u8 n -- n )  MOV16-MASK MOV16-OP LOOP-OPS ;
: FROM16S ( ptr u8 n -- n )  FROM16-MASK FROM16-OP LOOP-OPS ;
: WPOPS ( ptr u8 n -- n )  POP-MASK POP-OP WORD-OPS ;
: WPUSHES ( ptr u8 n -- n )  PUSH-MASK PUSH-OP WORD-OPS ;
: WMOV16S ( ptr u8 n -- n )  MOV16-MASK MOV16-OP WORD-OPS ;
: WFROM16S ( ptr u8 n -- n )  FROM16-MASK FROM16-OP WORD-OPS ;
: WBCONDS ( ptr u8 n -- n )  BCOND-MASK BCOND-OP WORD-OPS ;

public

\ ---- the subjects ------------------------------------------------------------
\ ( acc n ) both live in registers at `begin` and come back in them.
: R-EXACT ( n -- n ) 0 swap begin swap 1+ swap 1- dup 0= until drop ;
\ The two registers of `begin` come back exchanged: a 2-cycle.
: R-SWAP ( n -- n n ) dup 1+ begin swap 1- dup 0= until ;
\ `begin` records the accumulator alone; the body pulls the count below it.
: R-PULL ( n -- n ) 0 begin swap 1- swap 1+ over 0= until nip ;
\ The sum stays in its register through the else-less `if`: `if` records it,
\ the arm's increment comes back to it at `then`.
: J-THEN ( n n -- n ) + dup 0 > if 1+ then 3 * ;
\ Both arms push a constant: `else` records the true arm's register for it,
\ the false arm's constant lands there at `then`.
: J-ELSE ( n -- n n ) dup 0 > if 1 else 2 then ;
\ The arm exchanges the two registers `if` recorded: a 2-cycle at `then`.
: J-CYC ( n n -- n n ) over over < if swap then ;
\ `while` records the sum and the count; the exit path arrives in them.
: J-WHILE ( n n -- n ) + 0 begin over 0 > while swap 1- swap 1+ repeat nip ;
\ The inner `else` and both `then`s keep the one value in registers.
: J-NEST ( n -- n ) dup 0 > if dup 5 > if 1- else 1+ then then ;
\ The first `if`'s branch lands at `then`, right after `5 >`: the second `if`
\ may not rewind that compare, so only `0=` fuses.
: J-FENCE ( n -- n ) dup 0 < dup 0= if drop dup 5 > then if 100 + then ;
\ `then` lands on `5 >`'s compare, which the fused branch keeps.
: J-FUSED ( n -- n ) dup 0 > if 1+ then 5 > if 1 else 0 then ;

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

: TEST-JOIN-SHAPE ( -- )
   s" a forward join keeps the values its opener recorded in registers" T-LABEL
   s" TIER0-JOIN:J-THEN" WPOPS 2 T=
   s" TIER0-JOIN:J-THEN" WPUSHES 1 T=
   s" TIER0-JOIN:J-ELSE" WPOPS 1 T=
   s" TIER0-JOIN:J-ELSE" WPUSHES 2 T=
   s" TIER0-JOIN:J-CYC" WPOPS 2 T=
   s" TIER0-JOIN:J-CYC" WPUSHES 2 T=
   s" TIER0-JOIN:J-CYC" WMOV16S 1 T=
   s" TIER0-JOIN:J-CYC" WFROM16S 1 T=
   s" TIER0-JOIN:J-WHILE" WPOPS 2 T=
   s" TIER0-JOIN:J-WHILE" WPUSHES 1 T=
   s" TIER0-JOIN:J-NEST" WPOPS 1 T=
   s" TIER0-JOIN:J-NEST" WPUSHES 1 T=
   s" TIER0-JOIN:J-FENCE" WBCONDS 1 T=
   s" TIER0-JOIN:J-FUSED" WBCONDS 2 T= ;

: TEST-VALUES ( -- )
   s" every subject answers what its loop computes" T-LABEL
   3 R-EXACT  dup . cr  3 T=
   3 R-SWAP  over . dup . cr  0 T=  2 T=
   3 R-PULL  dup . cr  3 T=
   2 3 J-THEN  dup . cr  18 T=
   -4 1 J-THEN  dup . cr  -9 T=
   5 J-ELSE  over . dup . cr  1 T=  5 T=
   -5 J-ELSE  over . dup . cr  2 T=  -5 T=
   3 5 J-CYC  over . dup . cr  3 T=  5 T=
   5 3 J-CYC  over . dup . cr  3 T=  5 T=
   2 5 J-WHILE  dup . cr  7 T=
   7 J-NEST  dup . cr  6 T=
   3 J-NEST  dup . cr  4 T=
   -2 J-NEST  dup . cr  -2 T=
   -3 J-FENCE  dup . cr  97 T=
   7 J-FENCE  dup . cr  107 T=
   3 J-FENCE  dup . cr  3 T=
   5 J-FUSED  dup . cr  1 T=
   4 J-FUSED  dup . cr  0 T=
   -9 J-FUSED  dup . cr  0 T= ;

public

: RUN ( -- )
   T-RESET
   0 MISSING !
   TEST-SHAPE
   TEST-JOIN-SHAPE
   TEST-VALUES
   MISSING @ 0 T=
   T-REPORT ;

;package

TIER0-JOIN:RUN

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
\ `then` patches the first `if` to land past the compare's csetm, so its second
\ `if` keeps the flag, while J-FUSED's `then` lands on the compare itself and
\ both of its `if`s fuse. Every subject also runs and its answer is checked.
\
\ Inside a `do`/`?do` loop tier 0 keeps the innermost index in x29 (habu2.f
\ J-FRAME, J-I): `i` copies x29 into a pool register, and only a call or a
\ control-flow edge makes the compiler reload it from the memory frame, which
\ stays current because `loop` and `+loop` store every step. A reload, `i`'s
\ (LIDX-RELOAD?,) or the step's (LIDX-LOAD?,), addresses the frame through x16
\ (C-LOOP-TOP,) and then issues `ldr x29,[x16]`, the one instruction that
\ writes the cache from memory; every `loop` addresses its frame through x16 to
\ step it, so the address sequence alone does not mark a reload. L-TOP's `i` is
\ a copy alone, L-CALL's reloads once after the call, L-NOI's `loop` reloads the
\ index it steps after the call, and L-NEST's outer `i` reloads after the
\ inner loop closes. L-WIDE's wide local sends it through pass 2 (habu2.f
\ EM-P2-START), where each read of `k` spills the virtual stack and loads the
\ local into x9 (EM-P2-LOCREF): its `i` still copies x29 and never reloads.
\
\ `{:` binds its locals from the virtual stack's top (habu2.f
\ C-LBRACE-BIND-VS): a register entry is stored straight into its frame slot,
\ a constant through x16, and only the locals below the virtual stack pop from
\ memory, so entries deeper than the group stay in their registers. B-COMP's
\ computed value is one store and its copy below stays live, B-CON's constant
\ is one movz and no push, B-DEEP binds one local from a register and pops
\ the other, and B-LOOP's group inside a loop body leaves the back edge
\ nothing to push or pop. B-WIDE's wide local takes pass 2, which spills the
\ virtual stack before it carves.
\
\ Spans come from the DICTIONARY RECORD (src/habu/xref.f), as in
\ test/compiler/tier0-compare-branch.f.

require src/lib/test.f
require src/lib/errors.f
require src/habu/xref.f
require src/habu/code-bytes.f
require src/lib/adt/option.f

package TIER0-JOIN
private

$FFFFFFE0 constant POP-MASK     $F85F8E60 constant POP-OP      \ ldr xR,[x19,#-8]!
$FFFFFFE0 constant PUSH-MASK    $F8008660 constant PUSH-OP     \ str xR,[x19],#8
$FFE0FFFF constant MOV16-MASK   $AA0003F0 constant MOV16-OP    \ mov x16,xN
$FFFFFFE0 constant FROM16-MASK  $AA1003E0 constant FROM16-OP   \ mov xN,x16
$FF000010 constant BCOND-MASK   $54000000 constant BCOND-OP    \ b.cond
$FF000000 constant CBZ-MASK     $B4000000 constant CBZ-OP      \ cbz xR
$FFFFFFFF constant RELOAD-MASK  $F940021D constant RELOAD-OP   \ ldr x29,[x16]
$FFFFFFE0 constant MOVIDX-MASK  $AA1D03E0 constant MOVIDX-OP   \ mov xN,x29
$FFC003FF constant SPLD-MASK    $F94003E9 constant SPLD-OP     \ ldr x9,[sp,#off]
$FFC003E0 constant SPST-MASK    $F90003E0 constant SPST-OP     \ str xR,[sp,#off]
$FF800000 constant MOVZ-MASK    $D2800000 constant MOVZ-OP     \ movz xR,#imm

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
: WRELOADS ( ptr u8 n -- n )  RELOAD-MASK RELOAD-OP WORD-OPS ;
: WMOVIDXS ( ptr u8 n -- n )  MOVIDX-MASK MOVIDX-OP WORD-OPS ;
: WSPLDS ( ptr u8 n -- n )  SPLD-MASK SPLD-OP WORD-OPS ;
: WSPSTS ( ptr u8 n -- n )  SPST-MASK SPST-OP WORD-OPS ;
: WMOVZS ( ptr u8 n -- n )  MOVZ-MASK MOVZ-OP WORD-OPS ;

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
: L-INC ( n -- n ) 1+ ;
\ `i` right after `?do` copies the index register: nothing changed it.
: L-TOP ( n -- n ) 0 swap 0 ?do i + loop ;
\ The call may run a loop of its own: `i` reloads, then `loop` steps x29.
: L-CALL ( n -- n ) 0 swap 0 ?do L-INC i + loop ;
\ No `i`: `loop` after the call loads the index before it steps it.
: L-NOI ( n -- n ) 0 swap 0 ?do L-INC loop ;
\ The inner `i` copies; the outer `i` after the inner `loop` reloads.
: L-NEST ( n -- n ) 0 swap 0 ?do 2 0 ?do i + loop i + loop ;
\ Pass 2: both reads of `k` go through memory, `i` copies x29.
: L-WIDE ( option<n> n -- n )
   {: o:option<n> k:n :}
   0 k 0 ?do i k * + loop ;
\ The masked value binds from its register; the copy below it stays live.
: B-COMP ( n -- n ) dup 7 and {: k:n :} k + ;
\ The constant binds through x16.
: B-CON ( -- n ) 5 {: a:n :} a 1+ ;
\ `b` binds from the register `1+` left, `a` pops from memory.
: B-DEEP ( n n -- n ) 1+ {: a:n b:n :} a b - ;
\ The begin record's registers survive the bind for the back edge.
: B-LOOP ( n -- n ) 0 swap begin swap over + {: s:n :} s swap 1- dup 0= until drop ;
\ Pass 2: the incremented count is spilled before the carve pops it.
: B-WIDE ( option<n> n -- n )
   1+ {: o:option<n> k:n :}
   k ;

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

: TEST-INDEX-SHAPE ( -- )
   s" a loop index stays in x29 until a call or a join" T-LABEL
   s" TIER0-JOIN:L-TOP" WRELOADS 0 T=
   s" TIER0-JOIN:L-TOP" WMOVIDXS 1 T=
   s" TIER0-JOIN:L-CALL" WRELOADS 1 T=
   s" TIER0-JOIN:L-CALL" WMOVIDXS 1 T=
   s" TIER0-JOIN:L-NOI" WRELOADS 1 T=
   s" TIER0-JOIN:L-NEST" WRELOADS 1 T=
   s" TIER0-JOIN:L-NEST" WMOVIDXS 2 T=
   s" TIER0-JOIN:L-WIDE" WSPLDS 2 T=
   s" TIER0-JOIN:L-WIDE" WRELOADS 0 T=
   s" TIER0-JOIN:L-WIDE" WMOVIDXS 1 T= ;

: TEST-BIND-SHAPE ( -- )
   s" a locals group binds from the virtual stack, not through memory" T-LABEL
   s" TIER0-JOIN:B-COMP" WPOPS 1 T=
   s" TIER0-JOIN:B-COMP" WPUSHES 1 T=
   s" TIER0-JOIN:B-COMP" WSPSTS 1 T=
   s" TIER0-JOIN:B-CON" WPOPS 0 T=
   s" TIER0-JOIN:B-CON" WPUSHES 1 T=
   s" TIER0-JOIN:B-CON" WSPSTS 1 T=
   s" TIER0-JOIN:B-CON" WMOVZS 1 T=
   s" TIER0-JOIN:B-DEEP" WPOPS 2 T=
   s" TIER0-JOIN:B-DEEP" WPUSHES 1 T=
   s" TIER0-JOIN:B-DEEP" WSPSTS 2 T=
   s" TIER0-JOIN:B-LOOP" LOOP-LEN 0 T<>
   s" TIER0-JOIN:B-LOOP" POPS 0 T=
   s" TIER0-JOIN:B-LOOP" PUSHES 0 T= ;

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
   -9 J-FUSED  dup . cr  0 T=
   4 L-TOP  dup . cr  6 T=
   4 L-CALL  dup . cr  10 T=
   4 L-NOI  dup . cr  4 T=
   3 L-NEST  dup . cr  6 T=
   5 OPTION:SOME 4 L-WIDE  dup . cr  24 T=
   13 B-COMP  dup . cr  18 T=
   B-CON  dup . cr  6 T=
   10 3 B-DEEP  dup . cr  6 T=
   4 B-LOOP  dup . cr  10 T=
   5 OPTION:SOME 4 B-WIDE  dup . cr  5 T= ;

public

: RUN ( -- )
   T-RESET
   0 MISSING !
   TEST-SHAPE
   TEST-JOIN-SHAPE
   TEST-INDEX-SHAPE
   TEST-BIND-SHAPE
   TEST-VALUES
   MISSING @ 0 T=
   T-REPORT ;

;package

TIER0-JOIN:RUN

\ tier0-inline-prims.f - tier 0 copies a straight-line primitive's body into
\ its caller (src/habu/habu2.f EMIT-CINLINE) when it holds no call, return,
\ PC-relative address, literal load or branch leaving the body; any other body
\ keeps its call (a BL, or a B in tail position). ARM's default tier is 0, so
\ none is selected here. Spans come from the dictionary record (src/habu/xref.f)
\ the compiler resolves calls against.

require src/lib/test.f
require src/lib/errors.f
require src/habu/xref.f
require src/habu/code-bytes.f
require src/compiler/native/branch.f
require src/habu/address-carrier.f

\ A constant and a created word re-exported from another package: the alias of
\ the constant keeps its definer stamp, the alias of the created word does not.
package T0-FIX-SRC
public
700 constant PK
create PTB 2 cells allot
;package

package T0-FIX-OUT
public
EXPORT T0-FIX-SRC:PK
EXPORT T0-FIX-SRC:PTB
;package

package TIER0-INLINE-PRIMS
private

$D503201F constant ENTRY-NOP         \ nop: the entry slot of a leaf
$F81F0FFE constant ENTRY-SAVE        \ str x30,[sp,#-16]!: the entry slot of a caller
$FFFFFFE0 constant REG-MASK          \ clears an instruction's Rt field
$F85F8E60 constant STACK-POP         \ ldr xR,[x19,#-8]!
$F8008660 constant STACK-PUSH        \ str xR,[x19],#8
$FC008660 constant FLOAT-PUSH        \ str dR,[x19],#8
$FFC0001F constant CMPI-MASK         \ keeps cmp xN,#imm12's opcode, shift and Rd
$F100001F constant CMPI-OP           \ cmp xN,#imm12 (subs xzr,xN,#imm12)
$FFE0FC1F constant CMPR-MASK         \ keeps cmp xN,xM's opcode, shift, amount and Rd
$EB00001F constant CMPR-OP           \ cmp xN,xM (subs xzr,xN,xM)
$D65F03C0 constant RET-OP            \ ret
$FFFFFFFF constant WORD-MASK         \ keeps every bit
$F84107FE constant LINK-REST         \ ldr x30,[sp],#16
$FFC003FF constant ADDSP-MASK        \ keeps add sp,sp,#imm12's opcode and registers
$910003FF constant ADDSP-OP          \ add sp,sp,#imm12
$FFE00000 constant MOVW-MASK         \ keeps a move-wide word's opcode and shift
$FFFF0FE0 constant CSETM-MASK        \ keeps csetm xD's opcode, clears cond and Rd
$DA9F03E0 constant CSETM-OP          \ csetm xD,cond (csinv xD,xzr,xzr,!cond)
$FFE0FFE0 constant NEG-MASK          \ keeps neg's opcode, clears Rd and Rm
$CB0003E0 constant NEG-OP            \ neg xD,xM (sub xD,xzr,xM)
$FFE0FC00 constant SDIV-MASK         \ keeps sdiv's opcode, clears its registers
$9AC00C00 constant SDIV-OP           \ sdiv xd,xn,xm
$FFE0FFE0 constant MOV-MASK          \ keeps mov's opcode, clears Rd and Rm
$AA0003E0 constant MOV-OP            \ mov xD,xM (orr xD,xzr,xM)

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

\ How many BLs (jump false) or Bs (jump true) the named word holds into the
\ named target's span.
: BRANCHES-INTO ( ptr u8 n ptr u8 n bool -- n ) {: na:ptr nu:n ta:ptr tu:n jump:bool :}
   ta tu SPAN {: tlo:n thi:n :}
   na nu SPAN {: lo:n hi:n :}
   0
   hi lo ?do
      i CODE@ {: w:n :}
      jump if w NBR:B? else w NBR:BL? then if
         jump if i w NBR:B-TARGET else i w NBR:BL-TARGET then {: t:n :}
         t tlo >= t thi < and if 1+ then
      then
   4 +loop ;

: BLS ( ptr u8 n ptr u8 n -- n ) false BRANCHES-INTO ;
: BS ( ptr u8 n ptr u8 n -- n ) true BRANCHES-INTO ;

: CALLS-WORD ( ptr u8 n ptr u8 n -- n ) {: na:ptr nu:n ta:ptr tu:n :}
   na nu ta tu BLS  na nu ta tu BS + ;

: MASKED-OPS ( ptr u8 n n n -- n ) {: na:ptr nu:n mask:n op:n :}
   na nu SPAN {: lo:n hi:n :}
   0
   hi lo ?do
      i CODE@ mask and op = if 1+ then
   4 +loop ;

: STACK-OPS ( ptr u8 n n -- n )
   {: op:n :}
   REG-MASK op MASKED-OPS ;

: ENTRY@ ( ptr u8 n -- n )
   SPAN drop CODE@ ;

\ The named word's k-th instruction counted back from the end of its span.
: LAST@ ( ptr u8 n n -- n ) {: na:ptr nu:n k:n :}
   na nu SPAN nip k 4 * - CODE@ ;

: RETS ( ptr u8 n -- n ) WORD-MASK RET-OP MASKED-OPS ;

\ How many Bs the named word holds to its own entry.
: HOME-JUMPS ( ptr u8 n -- n ) {: na:ptr nu:n :}
   na nu SPAN {: lo:n hi:n :}
   0
   hi lo ?do
      i CODE@ {: w:n :}
      w NBR:B? if
         i w NBR:B-TARGET lo = if 1+ then
      then
   4 +loop ;

: INLINED ( ptr u8 n ptr u8 n -- ) CALLS-WORD 0 T= ;
: CALLED ( ptr u8 n ptr u8 n -- ) CALLS-WORD 1 T= ;

create T0-CELL 2 cells allot
create T0-TEXT 4 allot
2 PTR-U8-TABLE T0-TAB

public

\ ---- the subjects ------------------------------------------------------------
\ A body that only inlines stays a leaf: EM-COMPILE-RET rewrites its entry slot
\ to a nop, so the entry word is asserted as well as the call census.
: T0-ADD ( n n -- n ) + ;
: T0-ROT ( n n n -- n n n ) rot ;
: T0-FETCH ( ptr n -- n ) @ ;
: T0-CFETCH ( ptr u8 -- n ) c@ ;
: T0-CELLS ( n -- n ) cells ;
: T0-CELLPLUS ( n -- n ) cell+ ;
: T0-FIELD ( ptr ptr u8 n -- ptr ptr u8 ) ptr-field ;
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

\ The decoder's positive control, an ordinary call not in tail position: "no
\ BL" proves nothing unless the decoder sees this one.
: T0-CALLEE ( n -- n ) 1 lshift ;
: T0-CALLER ( n -- n ) T0-CALLEE 1+ ;

\ A definition whose last word is a call ends in a B to it (habu2.f
\ EM-COMPILE-SEMI) after the link and any local frame are released; no return
\ follows unless an `exit` needs one. When that call was the body's only one,
\ the body is a leaf: a nop entry and no restore.
\ Tail calls: plain, under a local frame, after a join, and beside an exit;
\ then the bodies that must keep the BL: a call followed by a join, and a
\ call that ends the arm a join lands after. T0-JOIN's `if` takes its only
\ cell, so its BL is the last word and only the join target keeps it a call.
\ T0-DOWN recurses as its last word. The ...2 bodies make a call before the
\ tail one, so they keep the frame; T0-DOESTAIL's clause is opened as having
\ called (its defining word's frame stands), so its one tail call keeps it too.
: T0-TAIL ( n -- n ) 1+ T0-CALLEE ;
: T0-TAIL2 ( n -- n ) T0-CALLEE T0-CALLEE ;
: T0-TAILLOC ( n -- n )
   {: a:n :}
   a 1+ T0-CALLEE ;
: T0-TAILLOC2 ( n -- n )
   {: a:n :}
   a T0-CALLEE T0-CALLEE ;
: T0-DOESTAIL ( n -- ) create , does> ( -- n ) @ T0-CALLEE ;
3 T0-DOESTAIL T0-DT
: T0-TAILIF ( n -- n ) dup 0< if negate then T0-CALLEE ;
: T0-TAILEX ( n -- n ) dup 0< if exit then T0-CALLEE ;
: T0-NOTAIL ( n -- n ) T0-CALLEE dup 0< if 1+ then ;
variable T0-SV
: T0-SIDE ( -- ) 7 T0-SV ! ;
: T0-JOIN ( n -- ) 0< if T0-SIDE then ;
: T0-DOWN ( n n -- n ) dup 0= if drop exit then 1- swap 1+ swap recurse ;

\ An inlined body inside a loop and inside a quotation.
: T0-SPIN ( n n n n -- n n n ) {: a:n b:n c:n k:n :}
   a b c k begin >r rot r> 1- dup 0= until drop ;
: T0-QROT ( n n n -- n n n ) [: rot ;] execute ;

\ Counted loops carry the virtual stack: what sits on it at `do` stays in
\ registers through the body, `leave`, a call, a local, `unloop exit` and a
\ throw, so the accumulator is pulled once at entry and pushed once at the end.
: T0-LOOPSUM ( n -- n ) 0 swap 0 ?do i + loop ;
: T0-LOOPLEAVE ( n -- n ) 0 swap 0 ?do i 5 = if leave then i + loop ;
: T0-LOOPDOWN ( -- n ) 0 0 10 ?do i + -1 +loop ;
: T0-LOOPNEST ( -- n ) 0 3 0 ?do 3 0 ?do i j * + loop loop ;
: T0-LOOPCALL ( n -- n ) 0 swap 0 ?do i T0-CALLEE + loop ;
: T0-LOOPEXIT ( n -- n ) 0 swap 0 ?do i 7 = if unloop exit then i + loop ;
: T0-LOOPLOC ( n -- n ) 0 swap 0 ?do i {: k:n :} k + loop ;
: T0-DOONE ( -- n ) 0 0 0 do 1+ loop ;
: T0-LOOPTHROW ( -- n ) 0 5 0 ?do i 3 = if 77 throw then i + loop ;

\ The call spills the virtual stack inside the loop, so `+` pulls both cells
\ again. The loop head recorded x9 for its one slot: the first pull is held
\ off it and the second takes it (jit.f NEED-ALLOC,), so the back edge needs no
\ copy, and the `dup`s share their registers: no movs.
: T0-PULLHOME ( n -- n ) 1+ begin dup T0-CALLEE + dup 100 > until ;

\ Arithmetic on cells the caller left on the data stack, one subject per entry
\ that pulls: binary, binary with an immediate, unary, shuffle, float.
: T0-SUB2 ( n n -- n ) - 3 * 1+ ;
: T0-IMM ( n -- n ) 4 - 3 * ;
: T0-UN ( n -- n ) 1+ 1+ ;
: T0-SHUF ( n n -- n ) swap - ;
: T0-FLT ( r r -- r ) f- 0.5 f+ ;

\ Memory words on the virtual stack: the address arithmetic and the loads run
\ on the pulled registers, with no push between them.
: T0-NEXT ( ptr n -- n ) cell+ @ 1+ ;
: T0-IDX ( ptr n n -- n ) cells + @ ;
: T0-BYTE ( ptr u8 -- n ) c@ 1+ ;
: T0-SLOT ( ptr ptr u8 n -- n ) ptr-field @ c@ ;

\ Comparisons against a literal top: one in 0..4095 is the immediate of a
\ cmp xN,#imm12; one outside it, or a register top, takes cmp xN,xM.
: T0-LT5 ( n -- n ) 5 < if 1 else 2 then ;
: T0-EQ5 ( n -- n ) 5 = if 1 else 2 then ;
: T0-NE5 ( n -- n ) 5 <> if 1 else 2 then ;
: T0-GT5 ( n -- n ) 5 > if 1 else 2 then ;
: T0-LE5 ( n -- n ) 5 <= if 1 else 2 then ;
: T0-GE5 ( n -- n ) 5 >= if 1 else 2 then ;
: T0-EQMAX ( n -- n ) 4095 = if 1 else 2 then ;
: T0-LTBIG ( n -- n ) 4096 < if 1 else 2 then ;
: T0-LTNEG ( n -- n ) -1 < if 1 else 2 then ;
: T0-LTREG ( n n -- n ) < if 1 else 2 then ;

\ Flags left on the stack: each is one csetm, never a cset and a neg.
: T0-FLAGEQ ( n n -- bool ) = ;
: T0-FLAGZEQ ( n -- bool ) 0= ;
: T0-FLAGZLT ( n -- bool ) 0< ;

\ Mentions of fixed values. A constant and a created word run while their user
\ compiles and fold into it (habu2.f EM-COMPILE-CALL), as a literal and as a
\ DATA address; a does> word, a defer and an EXPORT alias of a created word stay
\ calls. T0-NEAR sits right before its user, where its code may still be
\ writable; E-DIV-ZERO is baked. TEST-FIXED pins the folds with the call census.
1000 constant T0-K
variable T0-V
create T0-TB 2 cells allot
: T0-DOES ( n -- ) create , does> ( -- n ) @ 1+ ;
5 T0-DOES T0-D
defer T0-DEF ( -- n )
: T0-KEQ ( n -- n ) T0-K = if 1 else 2 then ;
: T0-KADD ( n -- n ) T0-K + ;
: T0-VGET ( -- n ) T0-V @ ;
: T0-TBGET ( -- n ) T0-TB cell+ @ ;
: T0-DGET ( -- n ) T0-D ;
: T0-DEFGET ( -- n ) T0-DEF ;
: T0-FAR ( -- n ) E-DIV-ZERO ;
: T0-XK ( n -- n ) T0-FIX-OUT:PK = if 1 else 2 then ;
: T0-XTB ( -- n ) T0-FIX-OUT:PTB @ ;
77 constant T0-NEAR
: T0-NEARU ( -- n ) T0-NEAR 1+ ;

\ Division by a literal (jit.f VDIVI-ENTRY): a divisor in 1..65535 divides in
\ place; a zero, wider or register divisor keeps the call and its zero test;
\ two literals fold, except over a zero divisor, which throws when it runs.
: T0-DIV256 ( n -- n ) 256 / ;
: T0-MOD256 ( n -- n ) 256 mod ;
: T0-MODR ( n n -- n ) mod ;
: T0-DIVMAX ( n -- n ) 65535 / ;
: T0-DIVWIDE ( n -- n ) 65536 / ;
: T0-DIV0 ( n -- n ) 0 / ;
: T0-MOD0 ( n -- n ) 0 mod ;
: T0-DIVK ( -- n ) 1000 256 / ;
: T0-DIVK0 ( -- n ) 1 0 / ;

\ Copies share a register (jit.f LVCOPY): `dup` and `over` emit no mov, and a
\ write to a shared register goes to a fresh one (LVDEST). A loop head keeps
\ its frame's registers distinct, so it splits a pair still shared there.
: T0-DUPINC ( n -- n n ) dup 1+ ;
: T0-OVERADD ( n n -- n n ) over + ;
: T0-DUPLT ( n -- n bool ) dup 5 < ;
: T0-DUPDUP ( n -- n ) dup dup + + ;
: T0-DUPIF ( bool -- bool n ) dup if 1 else 2 then ;
: T0-DUPCALL ( n -- n n ) dup T0-CALLEE ;
: T0-DUPFADD ( r -- r ) dup f+ ;
: T0-DUPLOOP ( n -- n ) dup begin 1- dup 0= until drop ;

private

: TEST-DECODER ( -- )
   s" the call decoder finds an ordinary call between two definitions" T-LABEL
   s" TIER0-INLINE-PRIMS:T0-CALLER" s" TIER0-INLINE-PRIMS:T0-CALLEE" CALLED ;

: TEST-INLINED ( -- )
   s" a straight-line primitive is copied, not called" T-LABEL
   s" TIER0-INLINE-PRIMS:T0-ADD" s" +" INLINED
   s" TIER0-INLINE-PRIMS:T0-ROT" s" rot" INLINED
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

: TAILS ( ptr u8 n -- ) {: na:ptr nu:n :}
   na nu s" TIER0-INLINE-PRIMS:T0-CALLEE" BLS 0 T=
   na nu s" TIER0-INLINE-PRIMS:T0-CALLEE" BS 1 T= ;

: KEEPS-CALL ( ptr u8 n ptr u8 n -- ) {: na:ptr nu:n ca:ptr cu:n :}
   na nu ca cu BLS 1 T=
   na nu ca cu BS 0 T=
   na nu RETS 1 T= ;

\ A body whose only call became its tail B: a nop entry and no link restore.
: TAIL-LEAF ( ptr u8 n -- ) {: na:ptr nu:n :}
   na nu ENTRY@ ENTRY-NOP T=
   na nu WORD-MASK LINK-REST MASKED-OPS 0 T= ;

: TEST-TAIL ( -- )
   s" an only call that is last is a B, with no frame and no return" T-LABEL
   s" TIER0-INLINE-PRIMS:T0-TAIL" TAILS
   s" TIER0-INLINE-PRIMS:T0-TAIL" TAIL-LEAF
   s" TIER0-INLINE-PRIMS:T0-TAIL" RETS 0 T=
   s" TIER0-INLINE-PRIMS:T0-TAIL" 1 LAST@ NBR:B? TTRUE
   s" a last call after another is a B after the link restore" T-LABEL
   s" TIER0-INLINE-PRIMS:T0-TAIL2" s" TIER0-INLINE-PRIMS:T0-CALLEE" BLS 1 T=
   s" TIER0-INLINE-PRIMS:T0-TAIL2" s" TIER0-INLINE-PRIMS:T0-CALLEE" BS 1 T=
   s" TIER0-INLINE-PRIMS:T0-TAIL2" RETS 0 T=
   s" TIER0-INLINE-PRIMS:T0-TAIL2" 1 LAST@ NBR:B? TTRUE
   s" TIER0-INLINE-PRIMS:T0-TAIL2" 2 LAST@ LINK-REST T=
   s" TIER0-INLINE-PRIMS:T0-TAIL2" ENTRY@ ENTRY-SAVE T=
   s" a local frame is released before the B, and before the link restore" T-LABEL
   s" TIER0-INLINE-PRIMS:T0-TAILLOC" TAILS
   s" TIER0-INLINE-PRIMS:T0-TAILLOC" TAIL-LEAF
   s" TIER0-INLINE-PRIMS:T0-TAILLOC" RETS 0 T=
   s" TIER0-INLINE-PRIMS:T0-TAILLOC" 2 LAST@ ADDSP-MASK and ADDSP-OP T=
   s" TIER0-INLINE-PRIMS:T0-TAILLOC2" RETS 0 T=
   s" TIER0-INLINE-PRIMS:T0-TAILLOC2" 2 LAST@ LINK-REST T=
   s" TIER0-INLINE-PRIMS:T0-TAILLOC2" 3 LAST@ ADDSP-MASK and ADDSP-OP T=
   s" TIER0-INLINE-PRIMS:T0-TAILLOC2" ENTRY@ ENTRY-SAVE T=
   s" a join that lands before the last call keeps the tail call" T-LABEL
   s" TIER0-INLINE-PRIMS:T0-TAILIF" TAILS
   s" TIER0-INLINE-PRIMS:T0-TAILIF" TAIL-LEAF
   s" TIER0-INLINE-PRIMS:T0-TAILIF" RETS 0 T=
   s" an exit beside the tail call keeps a return epilogue" T-LABEL
   s" TIER0-INLINE-PRIMS:T0-TAILEX" TAILS
   s" TIER0-INLINE-PRIMS:T0-TAILEX" TAIL-LEAF
   s" TIER0-INLINE-PRIMS:T0-TAILEX" RETS 1 T=
   s" a call before a join, or inside the arm a join lands after, stays a BL" T-LABEL
   s" TIER0-INLINE-PRIMS:T0-NOTAIL" s" TIER0-INLINE-PRIMS:T0-CALLEE" KEEPS-CALL
   s" TIER0-INLINE-PRIMS:T0-JOIN" s" TIER0-INLINE-PRIMS:T0-SIDE" KEEPS-CALL
   s" a last recurse is a B to the entry" T-LABEL
   s" TIER0-INLINE-PRIMS:T0-DOWN" s" TIER0-INLINE-PRIMS:T0-DOWN" BLS 0 T=
   s" TIER0-INLINE-PRIMS:T0-DOWN" HOME-JUMPS 1 T=
   s" TIER0-INLINE-PRIMS:T0-DOWN" TAIL-LEAF
   s" the tail-calling bodies answer what the calls answered" T-LABEL
   3 T0-TAIL 8 T=
   3 T0-TAIL2 12 T=
   3 T0-TAILLOC 8 T=
   3 T0-TAILLOC2 12 T=
   T0-DT 6 T=
   -3 T0-TAILIF 6 T=  3 T0-TAILIF 6 T=
   -3 T0-TAILEX -3 T=  3 T0-TAILEX 6 T=
   3 T0-NOTAIL 6 T=  -3 T0-NOTAIL -5 T=
   0 T0-SV !  3 T0-JOIN T0-SV @ 0 T=  -3 T0-JOIN T0-SV @ 7 T=
   0 7 T0-DOWN 7 T= ;

: TEST-LEAF ( -- )
   s" a body that only inlines stays a leaf; one that calls keeps its frame" T-LABEL
   s" TIER0-INLINE-PRIMS:T0-ROT" ENTRY@ ENTRY-NOP T=
   s" TIER0-INLINE-PRIMS:T0-ACA" ENTRY@ ENTRY-NOP T=
   s" TIER0-INLINE-PRIMS:T0-CALLER" ENTRY@ ENTRY-SAVE T= ;

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

\ One pop per stack operand and one push of the result: a copied memory body
\ would pop and push around itself, so a run of them would count more.
: MEM-OPS ( ptr u8 n n -- ) {: na:ptr nu:n pops:n :}
   na nu STACK-POP STACK-OPS pops T=
   na nu STACK-PUSH STACK-OPS 1 T= ;

: TEST-MEM ( -- )
   s" memory words load and address on registers, with no spill between them" T-LABEL
   s" TIER0-INLINE-PRIMS:T0-NEXT" 1 MEM-OPS
   s" TIER0-INLINE-PRIMS:T0-IDX" 2 MEM-OPS
   s" TIER0-INLINE-PRIMS:T0-BYTE" 1 MEM-OPS
   s" TIER0-INLINE-PRIMS:T0-SLOT" 2 MEM-OPS ;

: TEST-VALUES ( -- )
   s" the copied bodies answer what the calls answered" T-LABEL
   3 4 T0-ADD 7 T=
   1 2 3 T0-ROT {: x:n y:n z:n :}
   x 2 T=  y 3 T=  z 1 T=
   $55AA T0-CELL !
   T0-CELL T0-FETCH $55AA T=
   T0-CELL byte-view T0-CFETCH $AA T=
   3 T0-CELLS 24 T=
   3 T0-CELLPLUS 11 T=
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
   T0-TEXT T0-TAB 1 ptr-field !
   T0-TAB 1 T0-FIELD @ c@ 3 T=
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

: CMP-IMM ( ptr u8 n -- )
   {: na:ptr nu:n :}
   na nu CMPI-MASK CMPI-OP MASKED-OPS 1 T=
   na nu CMPR-MASK CMPR-OP MASKED-OPS 0 T= ;

: CMP-REG ( ptr u8 n -- )
   {: na:ptr nu:n :}
   na nu CMPI-MASK CMPI-OP MASKED-OPS 0 T=
   na nu CMPR-MASK CMPR-OP MASKED-OPS 1 T= ;

: TEST-CMPI ( -- )
   s" a comparison with a literal top in 0..4095 compares against an immediate" T-LABEL
   s" TIER0-INLINE-PRIMS:T0-LT5" CMP-IMM
   s" TIER0-INLINE-PRIMS:T0-EQ5" CMP-IMM
   s" TIER0-INLINE-PRIMS:T0-NE5" CMP-IMM
   s" TIER0-INLINE-PRIMS:T0-GT5" CMP-IMM
   s" TIER0-INLINE-PRIMS:T0-LE5" CMP-IMM
   s" TIER0-INLINE-PRIMS:T0-GE5" CMP-IMM
   s" TIER0-INLINE-PRIMS:T0-EQMAX" CMP-IMM
   s" a literal above 4095, a negative literal or a register top compares registers" T-LABEL
   s" TIER0-INLINE-PRIMS:T0-LTBIG" CMP-REG
   s" TIER0-INLINE-PRIMS:T0-LTNEG" CMP-REG
   s" TIER0-INLINE-PRIMS:T0-LTREG" CMP-REG
   s" each comparison answers at its boundary" T-LABEL
   4 T0-LT5 1 T=  5 T0-LT5 2 T=  6 T0-LT5 2 T=
   4 T0-EQ5 2 T=  5 T0-EQ5 1 T=  6 T0-EQ5 2 T=
   4 T0-NE5 1 T=  5 T0-NE5 2 T=  6 T0-NE5 1 T=
   4 T0-GT5 2 T=  5 T0-GT5 2 T=  6 T0-GT5 1 T=
   4 T0-LE5 1 T=  5 T0-LE5 1 T=  6 T0-LE5 2 T=
   4 T0-GE5 2 T=  5 T0-GE5 1 T=  6 T0-GE5 1 T=
   4094 T0-EQMAX 2 T=  4095 T0-EQMAX 1 T=  4096 T0-EQMAX 2 T=
   4095 T0-LTBIG 1 T=  4096 T0-LTBIG 2 T=  4097 T0-LTBIG 2 T=
   -7 T0-LTNEG 1 T=  -1 T0-LTNEG 2 T=  0 T0-LTNEG 2 T=
   4 5 T0-LTREG 1 T=  5 5 T0-LTREG 2 T=  6 5 T0-LTREG 2 T= ;

: CSETMS ( ptr u8 n -- n ) CSETM-MASK CSETM-OP MASKED-OPS ;
: NEGS ( ptr u8 n -- n ) NEG-MASK NEG-OP MASKED-OPS ;

: FLAG-CSETM ( ptr u8 n -- )
   {: na:ptr nu:n :}
   na nu CSETMS 1 T=
   na nu NEGS 0 T= ;

: TEST-FLAGS ( -- )
   s" 0= and 0< leave one csetm" T-LABEL
   s" TIER0-INLINE-PRIMS:T0-FLAGZEQ" FLAG-CSETM
   s" TIER0-INLINE-PRIMS:T0-FLAGZLT" FLAG-CSETM
   s" each flag is all ones or zero" T-LABEL
   3 3 T0-FLAGEQ bool>n -1 T=  3 4 T0-FLAGEQ bool>n 0 T=
   0 T0-FLAGZEQ bool>n -1 T=  7 T0-FLAGZEQ bool>n 0 T=
   -5 T0-FLAGZLT bool>n -1 T=  5 T0-FLAGZLT bool>n 0 T= ;

: TEST-FIXED ( -- )
   s" a constant mention folds: no call, and `K =` compares against an immediate" T-LABEL
   s" TIER0-INLINE-PRIMS:T0-KEQ" s" TIER0-INLINE-PRIMS:T0-K" INLINED
   s" TIER0-INLINE-PRIMS:T0-KEQ" CMP-IMM
   s" TIER0-INLINE-PRIMS:T0-KADD" s" TIER0-INLINE-PRIMS:T0-K" INLINED
   s" a constant just before its user, one baked in the engine and an alias fold" T-LABEL
   s" TIER0-INLINE-PRIMS:T0-NEARU" s" TIER0-INLINE-PRIMS:T0-NEAR" INLINED
   s" TIER0-INLINE-PRIMS:T0-FAR" s" E-DIV-ZERO" INLINED
   s" TIER0-INLINE-PRIMS:T0-XK" s" T0-FIX-SRC:PK" INLINED
   s" a created word's mention is one DATA carrier and no call" T-LABEL
   s" TIER0-INLINE-PRIMS:T0-VGET" s" TIER0-INLINE-PRIMS:T0-V" INLINED
   s" TIER0-INLINE-PRIMS:T0-VGET" MOVW-MASK ADDRESS-CARRIER:DATA-MOVZ2 MASKED-OPS 1 T=
   s" TIER0-INLINE-PRIMS:T0-TBGET" s" TIER0-INLINE-PRIMS:T0-TB" INLINED
   s" a does> word, a defer and an alias of a created word are still called" T-LABEL
   s" TIER0-INLINE-PRIMS:T0-DGET" s" TIER0-INLINE-PRIMS:T0-D" CALLED
   s" TIER0-INLINE-PRIMS:T0-DEFGET" s" TIER0-INLINE-PRIMS:T0-DEF" CALLED
   s" TIER0-INLINE-PRIMS:T0-XTB" s" T0-FIX-SRC:PTB" CALLED
   s" the folded mentions answer what the calls answered" T-LABEL
   999 T0-KEQ 2 T=  1000 T0-KEQ 1 T=
   5 T0-KADD 1005 T=
   T0-NEARU 78 T=
   T0-FAR E-DIV-ZERO T=
   700 T0-XK 1 T=  701 T0-XK 2 T=
   42 T0-V !  T0-VGET 42 T=
   9 T0-TB cell+ !  T0-TBGET 9 T=
   5 T0-FIX-SRC:PTB !  T0-XTB 5 T=
   T0-DGET 6 T=
   [: 41 ;] is T0-DEF  T0-DEFGET 41 T=
   [: 43 ;] is T0-DEF  T0-DEFGET 43 T= ;

\ STACK-PUSH and STACK-POP counts pin the carry; the values pin every exit.
: TEST-LOOPS ( -- )
   s" a counted loop carries the virtual stack through its body" T-LABEL
   s" TIER0-INLINE-PRIMS:T0-LOOPSUM" STACK-PUSH STACK-OPS 1 T=
   s" TIER0-INLINE-PRIMS:T0-LOOPSUM" STACK-POP STACK-OPS 1 T=
   s" TIER0-INLINE-PRIMS:T0-LOOPLEAVE" STACK-PUSH STACK-OPS 1 T=
   s" TIER0-INLINE-PRIMS:T0-LOOPLEAVE" STACK-POP STACK-OPS 1 T=
   s" TIER0-INLINE-PRIMS:T0-LOOPDOWN" STACK-PUSH STACK-OPS 1 T=
   s" TIER0-INLINE-PRIMS:T0-LOOPDOWN" STACK-POP STACK-OPS 0 T=
   s" carried loops answer through leave, nesting, calls, locals, exit and throw" T-LABEL
   10 T0-LOOPSUM 45 T=  0 T0-LOOPSUM 0 T=
   10 T0-LOOPLEAVE 10 T=  3 T0-LOOPLEAVE 3 T=
   T0-LOOPDOWN 55 T=
   T0-LOOPNEST 9 T=
   10 T0-LOOPCALL 90 T=
   10 T0-LOOPEXIT 21 T=  5 T0-LOOPEXIT 10 T=
   10 T0-LOOPLOC 45 T=
   T0-DOONE 1 T=
   [: T0-LOOPTHROW drop ;] 77 TTHROWSQ
   s" a pull after a call in a loop lands in its slot's register" T-LABEL
   s" TIER0-INLINE-PRIMS:T0-PULLHOME" MOV-MASK MOV-OP MASKED-OPS 0 T=
   3 T0-PULLHOME 108 T= ;

\ The named word holds no call to the dividing primitive and sdivs times.
: DIVIDES ( ptr u8 n ptr u8 n n -- ) {: na:ptr nu:n pa:ptr pu:n sdivs:n :}
   na nu pa pu INLINED
   na nu SDIV-MASK SDIV-OP MASKED-OPS sdivs T= ;

: TEST-DIVIDE ( -- )
   s" a literal divisor in 1..65535 divides in place, with no call" T-LABEL
   s" TIER0-INLINE-PRIMS:T0-DIV256" s" /" 1 DIVIDES
   s" TIER0-INLINE-PRIMS:T0-MOD256" s" mod" 1 DIVIDES
   s" TIER0-INLINE-PRIMS:T0-DIVMAX" s" /" 1 DIVIDES
   s" a zero, wider or register divisor keeps the call" T-LABEL
   s" TIER0-INLINE-PRIMS:T0-DIVWIDE" s" /" CALLED
   s" TIER0-INLINE-PRIMS:T0-DIV0" s" /" CALLED
   s" TIER0-INLINE-PRIMS:T0-MOD0" s" mod" CALLED
   s" TIER0-INLINE-PRIMS:T0-MODR" s" mod" CALLED
   s" two literals fold, unless the divisor is zero" T-LABEL
   s" TIER0-INLINE-PRIMS:T0-DIVK" s" /" 0 DIVIDES
   s" TIER0-INLINE-PRIMS:T0-DIVK0" s" /" CALLED
   s" the divisions truncate, as the baked words do, and refuse a zero divisor" T-LABEL
   1000 T0-DIV256 3 T=  -1000 T0-DIV256 -3 T=  255 T0-DIV256 0 T=
   1000 T0-MOD256 232 T=
   -1000 T0-MOD256  -1000 256 T0-MODR T=
   131070 T0-DIVMAX 2 T=  131072 T0-DIVWIDE 2 T=
   T0-DIVK 3 T=
   [: 5 T0-DIV0 drop ;] E-DIV-ZERO TTHROWSQ
   [: 5 T0-MOD0 drop ;] E-DIV-ZERO TTHROWSQ
   [: T0-DIVK0 drop ;] E-DIV-ZERO TTHROWSQ ;

: MOVS ( ptr u8 n -- n ) MOV-MASK MOV-OP MASKED-OPS ;

: TEST-SHARE ( -- )
   s" a copy shares its register; a write to a shared one goes to a fresh one" T-LABEL
   s" TIER0-INLINE-PRIMS:T0-DUPINC" MOVS 0 T=
   s" TIER0-INLINE-PRIMS:T0-OVERADD" MOVS 0 T=
   s" TIER0-INLINE-PRIMS:T0-DUPLT" MOVS 0 T=
   s" TIER0-INLINE-PRIMS:T0-DUPDUP" MOVS 0 T=
   s" TIER0-INLINE-PRIMS:T0-DUPIF" MOVS 0 T=
   s" TIER0-INLINE-PRIMS:T0-DUPCALL" MOVS 0 T=
   s" TIER0-INLINE-PRIMS:T0-DUPCALL" STACK-PUSH STACK-OPS 2 T=
   s" TIER0-INLINE-PRIMS:T0-DUPFADD" MOVS 0 T=
   s" a pair still shared at a loop head is split once, before the loop" T-LABEL
   s" TIER0-INLINE-PRIMS:T0-DUPLOOP" MOVS 1 T=
   s" each copy keeps its value when the other is written" T-LABEL
   3 T0-DUPINC 4 T= 3 T=
   3 4 T0-OVERADD 7 T= 3 T=
   3 T0-DUPLT TTRUE 3 T=  7 T0-DUPLT TFALSE 7 T=
   5 T0-DUPDUP 15 T=
   0 0= T0-DUPIF 1 T= TTRUE  1 0= T0-DUPIF 2 T= TFALSE
   3 T0-DUPCALL 6 T= 3 T=
   1.5 T0-DUPFADD 3.0 f= TTRUE
   3 T0-DUPLOOP 3 T= ;

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
   TEST-TAIL
   TEST-LEAF
   TEST-PULL
   TEST-MEM
   TEST-VALUES
   TEST-CMPI
   TEST-FLAGS
   TEST-FIXED
   TEST-LOOPS
   TEST-DIVIDE
   TEST-SHARE
   TEST-QDUP
   MISSING @ 0 T=
   T-REPORT ;

;package

TIER0-INLINE-PRIMS:RUN

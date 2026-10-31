\ t0.f - the tier 0 compiler's front end, package T0.
\
\ Compiles one definition's body text (BODYBUF at `;`, src/habu/definers.f
\ DEF-COMPILE) from the tape the checker's scan reports, through the step every
\ native compiler shares (src/compiler/native/definition.f), and emits through
\ src/habu/t0-emit.f to the selected emitter. A name, a tick's operand and
\ `is`'s operand lower from the record the checker bound, as tier 1's
\ src/compiler/native/hir-word.f RESOLVE-SITE does; spelling is looked up only
\ by IMM-TOKEN (CHECKER-OWNER:PARSE-IMM?) and VALIDATE (NFETCH-CHECK:TAGS).

require src/lib/prelude.f
require src/lib/errors.f
require src/core/bytes.f
require src/compiler/native/checker-owner.f
require src/compiler/native/dict.f
require src/core/prefix-boundary.f
require src/compiler/native/family.f
require src/compiler/native/trap.f
require src/compiler/native/fetch.f
require src/compiler/native/string.f
require src/compiler/native/publish.f
require src/compiler/native/definition.f
require src/compiler/native/callback-stub.f
require src/habu/t0-emit.f

package T0

\ ---- the tape ----------------------------------------------------------------
\ THE ORDINALS. The checker files what it proved about a token under that
\ token's ordinal (src/core/checker.f SCAN-REPORT), and the tape holds one row
\ per ordinal: the name is 0, the signature reports nothing, a string literal
\ with its opener is one, `[char]` with its operand is one, and a
\ tick's or `is`'s operand is one after its keyword. A quotation's first
\ ordinal is recorded where it is skipped; the does> clause's follows its
\ `does>`.
\
\ THE TAPE. Row k is the token the checker's scan reported as ordinal k (the
\ runtime compiler's observer lays it): its span, an offset and a length in
\ the definition's text, and the event the checker filed for it, a kind of
\ CHECKER-TAPE's and its two arguments. A string row keeps its body's length,
\ and an escaped one a copy of its decoded body in BODIES, since the checker's
\ buffer outlives no token.
PTR-VARIABLE TXT
DYNAMIC-BUFFER ROW-OFF n
DYNAMIC-BUFFER ROW-LEN n
DYNAMIC-BUFFER ROW-KIND n
DYNAMIC-BUFFER ROW-A0 n
DYNAMIC-BUFFER ROW-A1 n
DYNAMIC-BUFFER ROW-U n               \ a string's body length
DYNAMIC-BUFFER ROW-AT n              \ where an escaped string's body starts in BODIES, else -1
DYNAMIC-BUFFER BODIES u8
variable BODIES-N
variable ROWS                        \ the rows laid
variable SPLIT                       \ a does> definer's `does>` row, or -1
-1 constant K-NONE                   \ a row the checker filed no event for: the `does>`
0 constant CF-QUOT-OPEN              \ a K-CONTROL row's a0 for `[:` (src/core/checker.f CF-TOK?)
1 constant CF-QUOT-CLOSE             \ and for `;]`
2 constant CF-IF                     \ `if`, and so on in the checker's list
3 constant CF-ELSE
4 constant CF-THEN
5 constant CF-CASE
6 constant CF-OF
7 constant CF-ENDOF
8 constant CF-ENDCASE
10 constant CF-BEGIN
11 constant CF-UNTIL
12 constant CF-AGAIN
13 constant CF-WHILE
14 constant CF-REPEAT
15 constant CF-DO
16 constant CF-?DO
17 constant CF-LOOP
18 constant CF-+LOOP
19 constant CF-I
20 constant CF-J
21 constant CF-EXIT
22 constant CF-LEAVE
23 constant CF-UNLOOP
24 constant CF-RECURSE
25 constant CF-TOR                   \ `>r`, then `r>` and `r@` (RS-KEYWORD)
26 constant CF-RFROM
27 constant CF-RFETCH
PTR-VARIABLE BAD
variable BAD-LEN
variable MISSING                     \ nonzero: the refused token names a word compiled code cannot reach
variable ORD                         \ the ordinal of the row the walk reads next
variable CUT                         \ the byte just past `does> `, or 0

: TXT$ ( -- ptr u8 ) TXT @ ;

\ Row k's span, empty past the tape.
: ROW$ ( n -- ptr u8 n ) {: k:n :}
   k ROWS @ >= if TXT$ 0 exit then
   TXT$ k ROW-OFF @ +  k ROW-LEN @ ;

\ The row the walk reads next, which it then passes.
: NEXT$ ( -- ptr u8 n )
   ORD @ ROW$
   ORD @ ROWS @ < if 1 ORD +! then ;

: QUOT-END? ( n -- bool ) {: k:n :}
   k ROW-KIND @ CHECKER-TAPE:K-CONTROL =  k ROW-A0 @ CF-QUOT-CLOSE =  and ;

\ Whether the unit ends at the next row: the tape does, or a quotation's `;]`.
: AT-END? ( -- bool )
   ORD @ ROWS @ >= if true exit then
   ORD @ QUOT-END? ;

\ Throw code, naming the token for UNSUPPORTED$.
: REFUSE ( ptr u8 n n -- ) {: a:ptr u:n code:n :}
   a BAD !  u BAD-LEN !
   code throw ;

: CONTROL-FAULT ( -- ) ORD @ 1- ROW$ E-T0-CONTROL REFUSE ;

: UNSUPPORTED ( ptr u8 n -- ) E-T0-UNSUPPORTED REFUSE ;

\ ---- the control stack -------------------------------------------------------
\ THE CONTROL STACK holds each open structure's kind with the locals' frame
\ slots in scope where its block opened. `while` goes under its `begin`, as
\ Forth's orig/dest order has it, so `begin ... while ... while ... repeat
\ then` closes. A structure that does not close, or a loop word outside the
\ loops it needs, is E-T0-CONTROL, refused before the emitter is called: the
\ T0-EMIT structured words (src/habu/t0-emit.f CONTROL) trust this nesting.
\ The refusal names the closer, or the token that ended a unit with a structure
\ still open: the stack keeps no opener's row.
0 constant CS-IF
1 constant CS-ELSE
2 constant CS-BEGIN
3 constant CS-WHILE
4 constant CS-DO
5 constant CS-CASE
6 constant CS-OF
7 constant CS-ENDOF
8 constant CS-MATCH

DYNAMIC-BUFFER CS-KIND n
DYNAMIC-BUFFER CS-LOC n              \ the locals' slots in scope where the block opened
DYNAMIC-BUFFER CS-FAM n              \ a MATCH's family
DYNAMIC-BUFFER CS-WID n              \ a MATCH's cells, the tag on top
DYNAMIC-BUFFER CS-LAY n              \ layout k's vregs from k*VREGS, bottom first
DYNAMIC-BUFFER CS-LAY-N n            \ layout k's count, -1 unrecorded
variable CS-N
variable DEAD                        \ nonzero: the fallthrough is unreachable

: LAY-ROOM ( n -- ) {: k:n :}
   k 1+ T0-EMIT:VREGS * CS-LAY-RESERVE  k 1+ CS-LAY-N-RESERVE ;

: LAY@ ( n n -- n ) {: k:n j:n :} k T0-EMIT:VREGS * j + CS-LAY @ ;
: LAY! ( n n n -- ) {: v:n k:n j:n :} v k T0-EMIT:VREGS * j + CS-LAY ! ;

\ The innermost open structure with a recorded layout, or -1.
: STEERING ( -- n )
   CS-N @ 1- begin dup 0 >= if dup CS-LAY-N @ 0< else false then while 1- repeat ;

\ ---- the virtual stack -------------------------------------------------------
\ THE VIRTUAL STACK. The top of the data stack lives in at most T0-EMIT:VREGS
\ entries, each a vreg or a constant; below them it is in memory. Paths that
\ meet at an `else` or `then`, a `begin` loop's head and exits and a counted
\ loop's head and exit keep their values in registers: the opener records a
\ layout (SNAPSHOT, every entry in a vreg of its own) and each joining path
\ reconciles to it (RECONCILE). Every call, `case` and MATCH join spills the
\ whole virtual stack, so those paths meet with it empty. Entries may share a
\ vreg (`dup`, `over`, a wide move's cell), and a vreg is free only once no
\ entry holds it (RELEASE). An in-place write takes the entry through OWN,
\ which copies a shared vreg first; a whole overwrite takes it through
\ WRITE-TARGET, which relabels it to a fresh vreg. A comparison followed by
\ `if`, `while` or `until` leaves its condition in the flags; otherwise it
\ pushes its flag into a fresh vreg.
0 constant K-REG
1 constant K-CONST

DYNAMIC-BUFFER VS-KIND n            \ reserved to T0-EMIT:VREGS by RESET
DYNAMIC-BUFFER VS-VAL n
variable VS-N
variable FREE                       \ bit k set: no entry holds vreg k

: ALL-FREE ( -- n ) 1 T0-EMIT:VREGS lshift 1- ;

: TOP ( -- n ) VS-N @ 1- ;

: REG? ( n -- bool ) VS-KIND @ K-REG = ;

: VREG@ ( n -- T0-EMIT:vreg ) VS-VAL @ T0-EMIT:>VREG ;

-1 constant NO-ENTRY                \ an entry index HOLDERS skips none for

\ How many entries other than entry x hold vreg v.
: HOLDERS ( n n -- n ) {: v:n x:n :}
   0 VS-N @ 0 ?do i x <> i REG? and i VS-VAL @ v = and if 1+ then loop ;

\ Free a vreg once no entry holds it; one an entry still holds stays allocated.
: RELEASE ( T0-EMIT:vreg -- )
   T0-EMIT:VREG>N {: n:n :}
   n NO-ENTRY HOLDERS 0<> if exit then
   1 n lshift FREE @ or FREE ! ;

: ENTRY! ( n n n -- ) {: kind:n val:n k:n :}
   kind k VS-KIND !  val k VS-VAL ! ;

: COPY-ENTRY ( n n -- ) {: from:n to:n :}
   from VS-KIND @ from VS-VAL @ to ENTRY! ;

: REMOVE ( n -- ) {: k:n :}
   k REG? k VS-VAL @ {: reg:bool v:n :}
   VS-N @ k 1+ ?do i i 1- COPY-ENTRY loop
   VS-N @ 1- VS-N !
   reg if v T0-EMIT:>VREG RELEASE then ;

: SPILL-ENTRY ( n -- ) {: k:n :}
   k REG? if k VREG@ k T0-EMIT:SPILL exit then
   k VS-VAL @ k T0-EMIT:SPILL-LIT ;

: PUSH-ENTRY ( n -- ) {: n:n :}
   0 REG? if 0 VREG@ n T0-EMIT:PUSH exit then
   0 VS-VAL @ n T0-EMIT:PUSH-LIT ;

: EVICT ( -- )
   1 PUSH-ENTRY
   0 REMOVE ;

: ROOM ( -- )
   VS-N @ T0-EMIT:VREGS = if EVICT then ;

\ Vreg n, which no entry holds, allocated.
: CLAIM ( n -- T0-EMIT:vreg ) {: n:n :}
   1 n lshift invert FREE @ and FREE !
   n T0-EMIT:>VREG ;

: LOWEST ( n -- T0-EMIT:vreg ) {: m:n :}
   0 begin 1 over lshift m and 0= while 1+ repeat CLAIM ;

\ A vreg no entry or TAKE holds. A caller holding a taken vreg ALLOCs only for
\ an entry with none, at most VREGS-1 entries being left, so a vreg is free;
\ with none taken, entries holding every vreg hold one each, so EVICT frees the
\ bottom one's.
: ALLOC ( -- T0-EMIT:vreg )
   FREE @ 0= if EVICT then FREE @ LOWEST ;

: PUSH-REG ( T0-EMIT:vreg -- ) {: r:T0-EMIT:vreg :}
   K-REG r T0-EMIT:VREG>N VS-N @ ENTRY!
   VS-N @ 1+ VS-N ! ;

: PUSH-CONST ( n -- ) {: v:n :}
   ROOM
   K-CONST v VS-N @ ENTRY!
   VS-N @ 1+ VS-N ! ;

: SPILL-ALL ( -- )
   VS-N @ {: n:n :}
   n 1 ?do n i - SPILL-ENTRY loop
   n 0 > if n PUSH-ENTRY then
   0 VS-N !
   ALL-FREE FREE ! ;

\ The vreg layout s names for entry j, counted from the bottom as the JIT's
\ snapshot frame counts its slots, or -1 for none or an entry above it.
: AIM ( n n -- n ) {: s:n j:n :}
   s 0< if -1 exit then
   j s CS-LAY-N @ >= if -1 exit then
   s j LAY@ ;

\ The vregs layout s names for entries 0 .. j-1.
: HELD-BACK ( n n -- n ) {: s:n j:n :}
   0 j 0 ?do s i AIM dup 0< if drop else 1 swap lshift or then loop ;

\ Steers a fill as the arm64 JIT's NEED-ALLOC, (src/habu/jit.f) steers a pull:
\ a value filled after a call inside a loop or an `if` lands in the vreg the
\ back edge or `then` needs. ENSURE asks for at most VREGS entries, so with the
\ held-back vregs aside one is free for LOWEST and nothing is evicted.
: STEER ( n n -- T0-EMIT:vreg ) {: s:n j:n :}
   s j AIM {: g:n :}
   g 0< if false else 1 g lshift FREE @ and 0<> then if g CLAIM exit then
   s j HELD-BACK invert FREE @ and LOWEST ;

: FILL-ENTRY ( n n n -- ) {: s:n k:n m:n :}
   s k STEER {: r:T0-EMIT:vreg :}
   k 0= if r m T0-EMIT:POP else r k m - T0-EMIT:FILL then
   K-REG r T0-EMIT:VREG>N k ENTRY! ;

: ENSURE ( n -- ) {: k:n :}
   VS-N @ k >= if exit then
   k VS-N @ - {: m:n :}
   VS-N @ 0 ?do VS-N @ 1- i - dup m + COPY-ENTRY loop
   STEERING {: s:n :}
   m 0 ?do s m 1- i - m FILL-ENTRY loop
   VS-N @ m + VS-N ! ;

\ A constant entry means a vreg is free, so ALLOC evicts nothing and k stays put.
: MAT ( n -- T0-EMIT:vreg ) {: k:n :}
   k REG? if k VREG@ exit then
   ALLOC {: r:T0-EMIT:vreg :}
   k VS-VAL @ r T0-EMIT:LIT
   K-REG r T0-EMIT:VREG>N k ENTRY!
   r ;

\ The top entry in a vreg, taken off the virtual stack. The vreg stays allocated
\ until the caller's next SPILL-ALL or RELEASE, and an entry below may still
\ hold it: the caller writes it only once no entry holds it.
: TAKE ( -- T0-EMIT:vreg )
   1 ENSURE
   TOP MAT {: r:T0-EMIT:vreg :}
   VS-N @ 1- VS-N !
   r ;

: SHARED? ( n n -- bool ) {: k:n x:n :} k VS-VAL @ x HOLDERS 1 > ;

\ A shared entry means a vreg is free, so ALLOC evicts nothing and k stays put.
: RELABEL ( n -- T0-EMIT:vreg ) {: k:n :}
   ALLOC {: d:T0-EMIT:vreg :}
   K-REG d T0-EMIT:VREG>N k ENTRY!
   d ;

\ Entry k in a vreg no entry other than k and x holds, for an operation that
\ writes it in place (d op= s) and consumes entry x, NO-ENTRY when it consumes
\ none: a vreg shared with a third entry is copied into a fresh one first.
: OWN ( n n -- T0-EMIT:vreg ) {: k:n x:n :}
   k MAT {: r:T0-EMIT:vreg :}
   k x SHARED? 0= if r exit then
   k RELABEL {: d:T0-EMIT:vreg :}
   d r T0-EMIT:MOV
   d ;

\ Entry k's vreg for an operation that overwrites it whole and reads the old
\ value only as an input register: a shared entry gets a fresh vreg, and
\ nothing is copied into it.
: WRITE-TARGET ( n -- T0-EMIT:vreg ) {: k:n :}
   k MAT {: r:T0-EMIT:vreg :}
   k NO-ENTRY SHARED? 0= if r exit then
   k RELABEL ;

\ ---- inline operations -------------------------------------------------------
: CONST? ( n -- bool ) VS-KIND @ K-CONST = ;

: IMM? ( n -- bool ) {: k:n :}
   k CONST? if k VS-VAL @ T0-EMIT:IMM? exit then
   false ;

\ A local cannot hold a quotation, so the folding words take theirs on the
\ stack, under the rest.
: FOLD2 ( [ n n -- n ] -- )
   TOP 1- VS-VAL @  TOP VS-VAL @  rot execute {: v:n :}
   VS-N @ 2 - VS-N !
   v PUSH-CONST ;

: CONST2? ( -- bool ) TOP 1- CONST? TOP CONST? and ;

\ The second entry, in a vreg no entry but the top one holds, holds the result;
\ the top entry, which may share that vreg (`dup *`), goes.
: INTO-SECOND ( -- T0-EMIT:vreg T0-EMIT:vreg )
   TOP 1- TOP OWN  TOP MAT ;

: ALU2 ( [ n n -- n ] T0-EMIT:alu bool -- ) {: o:T0-EMIT:alu comm:bool :}
   2 ENSURE
   CONST2? if FOLD2 exit then
   drop
   TOP IMM? if
      TOP 1- NO-ENTRY OWN TOP VS-VAL @ o T0-EMIT:OP-LIT
      TOP REMOVE exit
   then
   comm TOP 1- IMM? and if
      TOP NO-ENTRY OWN TOP 1- VS-VAL @ o T0-EMIT:OP-LIT
      TOP 1- REMOVE exit
   then
   INTO-SECOND {: d:T0-EMIT:vreg s:T0-EMIT:vreg :}
   d s o T0-EMIT:OP
   TOP REMOVE ;

: MUL2 ( -- )
   2 ENSURE
   CONST2? if [: * ;] FOLD2 exit then
   TOP IMM? if
      TOP 1- NO-ENTRY OWN TOP VS-VAL @ T0-EMIT:MUL-LIT
      TOP REMOVE exit
   then
   TOP 1- IMM? if
      TOP NO-ENTRY OWN TOP 1- VS-VAL @ T0-EMIT:MUL-LIT
      TOP 1- REMOVE exit
   then
   INTO-SECOND {: d:T0-EMIT:vreg s:T0-EMIT:vreg :}
   d s T0-EMIT:MULX
   TOP REMOVE ;

: DIVISOR? ( -- bool )
   VS-N @ 0= if false exit then
   TOP CONST? 0= if false exit then
   TOP VS-VAL @ {: v:n :}
   v 0 <> v -1 <> and ;

\ The flag: whether a literal divisor other than 0 and -1 claimed the token
\ (t0-emit.f DIVISION); any other divisor is left to the kernel row's call. The
\ fold runs the engine's own `/` or `mod`, which truncates toward zero as every
\ kernel's rows do.
: DIV2 ( [ n n -- n ] [ T0-EMIT:vreg n -- ] -- bool )
   DIVISOR? 0= if 2drop false exit then
   2 ENSURE
   CONST2? if drop FOLD2 true exit then
   nip TOP 1- NO-ENTRY OWN TOP VS-VAL @ rot execute
   TOP REMOVE true ;

: SHIFT2 ( [ n n -- n ] T0-EMIT:shifter -- ) {: o:T0-EMIT:shifter :}
   2 ENSURE
   CONST2? if FOLD2 exit then
   drop
   TOP CONST? if
      TOP 1- NO-ENTRY OWN TOP VS-VAL @ o T0-EMIT:SHIFT-LIT
      TOP REMOVE exit
   then
   INTO-SECOND {: d:T0-EMIT:vreg s:T0-EMIT:vreg :}
   d s o T0-EMIT:SHIFT
   TOP REMOVE ;

\ A cell is 8 bytes on every T0 target.
3 constant CELL-SHIFT

: CELLS-OP ( -- ) CELL-SHIFT PUSH-CONST [: lshift ;] T0-EMIT:SH-LEFT SHIFT2 ;

: REMOVE2 ( -- ) TOP REMOVE TOP REMOVE ;

: FOLD1 ( [ n -- n ] -- ) TOP VS-VAL @ swap execute TOP VS-VAL ! ;

: UNARY ( [ n -- n ] [ T0-EMIT:vreg -- ] -- )
   1 ENSURE
   TOP CONST? if drop FOLD1 exit then
   nip TOP NO-ENTRY OWN swap execute ;

: FETCH ( T0-EMIT:width -- ) {: w:T0-EMIT:width :}
   1 ENSURE
   TOP MAT {: a:T0-EMIT:vreg :}
   TOP WRITE-TARGET {: d:T0-EMIT:vreg :}
   d a w T0-EMIT:LOAD ;

\ ---- the stack shuffles ------------------------------------------------------
: COPY ( n -- ) {: k:n :}
   k CONST? if k VS-VAL @ PUSH-CONST exit then
   VS-N @ T0-EMIT:VREGS = if EVICT k 1- else k then {: j:n :}
   j VREG@ PUSH-REG ;

: SWAP-AT ( n n -- ) {: a:n b:n :}
   a VS-KIND @ a VS-VAL @ {: ka:n va:n :}
   b a COPY-ENTRY
   ka va b ENTRY! ;

: DUP1 ( -- ) 1 ENSURE TOP COPY ;
: OVER1 ( -- ) 2 ENSURE TOP 1- COPY ;
: NIP1 ( -- ) 2 ENSURE TOP 1- REMOVE ;
: SWAP1 ( -- ) 2 ENSURE TOP 1- TOP SWAP-AT ;
: ROT1 ( -- ) 3 ENSURE TOP 2 - TOP 1- SWAP-AT TOP 1- TOP SWAP-AT ;
: TUCK1 ( -- ) SWAP1 OVER1 ;

: DROP1 ( -- )
   VS-N @ 0= if -1 T0-EMIT:DSP+ exit then
   TOP REMOVE ;

\ ---- the scans' measures -----------------------------------------------------
\ A stack word moves whole values, by the cells the checker measured for each
\ operand at the token (position 0 the top; r> r@ 2r> 2r@ from the return row).
\ An unmeasured operand is one cell: the checker files only layout values. `>r`
\ moves a value's cells as one cell each would, so they sit reversed on the
\ return stack and `r>` restores their order. `@` and `!` move the measured
\ cells as tier 1's WIDE-LOAD and WIDE-STORE (src/compiler/native/elaborate.f),
\ `!` by the engine's guarded store once per cell. An `@` with a layout fact
\ (NFETCH:AT) calls NFETCH-CHECK:TAGS first, as tier 1's VALIDATE-FETCH; a fact
\ whose width is not the token's is E-T0-BUNDLE. Any other inlined operation
\ over a width other than one cell, and `@` or `!` under one cell, is
\ E-T0-BUNDLE, as tier 1's DO-OP refuses it.
\
\ ON-DONE keeps each scan's measures because a does> definer's clause scan
\ replaces the head's facts before the walk. A row: the token's offset in the
\ whole text, the operand position, its cells. Binds: the head's or the body's
\ first, then the clause's.
DYNAMIC-BUFFER MEASURE-AT n
DYNAMIC-BUFFER MEASURE-POS n
DYNAMIC-BUFFER MEASURE-CELLS n
variable MEASURES
DYNAMIC-BUFFER BIND-WIDTHS n
variable BIND-COUNT
variable HEAD-BINDS                  \ the binds of the scan at the text's start

: OPERAND-CELLS ( n n -- n ) {: at:n pos:n :}
   MEASURES @ 0 ?do
      i MEASURE-AT @ at =  i MEASURE-POS @ pos =  and if i MEASURE-CELLS @ unloop exit then
   loop
   1 ;

: SPLIT-BINDS ( n -- n n ) {: at:n :}
   CUT @ 0<>  at CUT @ >=  and if HEAD-BINDS @  BIND-COUNT @ HEAD-BINDS @ -  exit then
   0 HEAD-BINDS @ ;

\ ---- whole-width moves -------------------------------------------------------
DYNAMIC-BUFFER WS n                  \ the operands' cells, position 0 the top
variable WS-K                        \ operands the word moves

: WIDTHS ( ptr u8 n -- ) {: a:ptr k:n :}
   a TXT$ - {: at:n :}
   k WS-RESERVE
   k 0 ?do at i OPERAND-CELLS i WS ! loop
   k WS-K ! ;

: WIDE? ( -- bool )
   false WS-K @ 0 ?do i WS @ 1 > or loop ;

: DEPTH ( n -- n ) 0 swap 0 ?do i WS @ + loop ;

: WS-SWAP ( -- ) 0 WS @ 1 WS @ 0 WS ! 1 WS ! ;

: DEEP ( n -- ) {: d:n :}
   ROOM
   d VS-N @ < if TOP d - COPY exit then
   ALLOC {: r:T0-EMIT:vreg :}
   r d VS-N @ - 1+ negate T0-EMIT:FILL
   r PUSH-REG ;

\ Push every cell of operand k, with p cells pushed above the operands so far.
: PUT-OP ( n n -- n ) {: p:n k:n :}
   p k DEPTH + k WS @ + 1- {: d:n :}
   k WS @ 0 ?do d DEEP loop
   p k WS @ + ;

\ Remove the c cells under the top o.
: NIP-CELLS ( n n -- ) {: o:n c:n :}
   c 0= if exit then
   o VS-N @ <= if
      VS-N @ o - c min {: v:n :}
      v 0 ?do TOP o - REMOVE loop
      c v > if v c - T0-EMIT:DSP+ then
      exit
   then
   SPILL-ALL
   o 0 ?do
      ALLOC {: r:T0-EMIT:vreg :}
      r i o - T0-EMIT:FILL
      r i o - c - T0-EMIT:SPILL
      r RELEASE
   loop
   c negate T0-EMIT:DSP+ ;

\ How many leading outputs are the bottom operands in their order.
: KEPT ( ptr u8 n -- n ) {: outs:ptr u:n :}
   WS-K @ u min {: m:n :}
   m 0 ?do outs i + c@ $30 -  WS-K @ 1- i -  <> if i unloop exit then loop
   m ;

\ The WS-K operands replaced by outs, the operand positions of the outputs
\ from the bottom as digits: the kept bottom ones stay, the rest are copied.
: MOVE-WIDE ( ptr u8 n -- ) {: outs:ptr u:n :}
   outs u KEPT {: j:n :}
   WS-K @ j - DEPTH {: c:n :}
   0 u j ?do outs i + c@ $30 - PUT-OP loop {: o:n :}
   o c NIP-CELLS ;

\ A shuffle with its entry move over one-cell operands.
: SHUFFLE ( [ -- ] ptr u8 n ptr u8 n -- ) {: a:ptr k:n outs:ptr u:n :}
   a k WIDTHS
   WIDE? if drop outs u MOVE-WIDE exit then
   execute ;

: SWAP-OP ( -- ) WIDE? if s" 01" MOVE-WIDE exit then SWAP1 ;

\ ---- the locals --------------------------------------------------------------
\ Each `{: ... :}` group binds its names once, left to right from the deepest
\ cell, into slots of the body's machine-stack frame; a local holds the cells
\ the checker measured for its bind, and T0 never reads a type annotation. A
\ bind past the scan's count, or a body that binds fewer than it counts, is
\ E-T0-BINDS, as tier 1's BINDS-CK; a body that binds nothing reads none and
\ is not checked. A local is in scope from its group to the closer of the
\ structure it binds in, as the checker's CF-LOC-REST ends it; `while` keeps
\ the loop's names, as tier 1's borrow does. A dead local's slots go to the
\ next group. A local is its bind sequence, the checker's number for it in the
\ unit's scan: a K-LOCAL-DECL row names the next one, a K-LOCAL-REF row the
\ one it reads.
DYNAMIC-BUFFER L-SLOT n              \ by sequence: the first of its frame slots
DYNAMIC-BUFFER L-WIDTH n             \ by sequence: its cells
variable L-N                         \ the frame slots the locals in scope hold
variable G-CELLS                     \ the cells the open group's names hold so far
variable BINDS                       \ binds so far: the next one's sequence
variable CLAUSE                      \ a does> definer's clause unit, else -1

: DECLARE ( n n -- ) {: seq:n at:n :}
   at SPLIT-BINDS {: first:n count:n :}
   seq count >= if E-T0-BINDS throw then
   first seq + BIND-WIDTHS @ {: w:n :}
   seq 1+ BINDS !
   seq 1+ L-SLOT-RESERVE  seq 1+ L-WIDTH-RESERVE
   L-N @ G-CELLS @ +  seq L-SLOT !  w seq L-WIDTH !
   w G-CELLS +! ;

\ Pop c cells into slots base .. base+c-1, the top cell into the last.
: BIND-CELLS ( n n -- ) {: base:n c:n :}
   VS-N @ c min {: v:n :}
   v 0 ?do
      TOP MAT  base c + 1- i -  T0-EMIT:SLOT-STORE
      TOP REMOVE
   loop
   c v ?do
      ALLOC {: r:T0-EMIT:vreg :}
      i v - 1+ {: d:n :}
      i c 1- = if r d T0-EMIT:POP else r d negate T0-EMIT:FILL then
      r  base c + 1- i -  T0-EMIT:SLOT-STORE
      r RELEASE
   loop ;

\ The group's names come into scope only once their cells are bound.
: CLOSE-GROUP ( -- )
   L-N @ {: base:n :}
   G-CELLS @ {: cells:n :}
   base cells + T0-EMIT:SLOTS
   base cells BIND-CELLS
   base cells + L-N !
   0 G-CELLS ! ;

: DO-LOCAL ( n -- ) {: k:n :}
   k L-WIDTH @ 0 ?do
      ROOM ALLOC {: r:T0-EMIT:vreg :}
      r k L-SLOT @ i + T0-EMIT:SLOT-LOAD
      r PUSH-REG
   loop ;

\ ---- comparisons -------------------------------------------------------------
\ The compare's operands are removed, so a vreg is free and ALLOC emits nothing
\ between the compare and SET-FLAG, as T0-EMIT:CMP requires.
: PUSH-FLAG ( T0-EMIT:cc -- ) {: c:T0-EMIT:cc :}
   ALLOC {: d:T0-EMIT:vreg :}
   d c T0-EMIT:SET-FLAG
   d PUSH-REG ;

\ One more than the condition (T0-EMIT:CC>N) of a compare whose flag is still
\ in the condition codes, 0 none. A compare leaves it only when the next token
\ is `if`, `while` or `until`, whose DO-IF, DO-WHILE or DO-UNTIL takes it, so
\ no other word ever finds it set.
variable PENDING

: BRANCH-NEXT? ( -- bool )
   ORD @ {: k:n :}
   k ROWS @ >= if false exit then
   k ROW-KIND @ CHECKER-TAPE:K-CONTROL <> if false exit then
   k ROW-A0 @ {: code:n :}
   code CF-IF =  code CF-WHILE = or  code CF-UNTIL = or ;

: CMP-FLAG ( T0-EMIT:cc -- ) {: c:T0-EMIT:cc :}
   BRANCH-NEXT? if c T0-EMIT:CC>N 1+ PENDING ! exit then
   c PUSH-FLAG ;

: CMP2 ( [ n n -- n ] T0-EMIT:cc -- ) {: c:T0-EMIT:cc :}
   2 ENSURE
   CONST2? if FOLD2 exit then
   drop
   TOP IMM? if
      TOP 1- MAT TOP VS-VAL @ T0-EMIT:CMP-LIT
      REMOVE2 c CMP-FLAG exit
   then
   TOP 1- IMM? if
      TOP MAT TOP 1- VS-VAL @ T0-EMIT:CMP-LIT
      REMOVE2 c T0-EMIT:CC-SWAP CMP-FLAG exit
   then
   TOP 1- MAT TOP MAT T0-EMIT:CMP
   REMOVE2 c CMP-FLAG ;

: CMP0 ( [ n -- n ] T0-EMIT:cc -- ) {: c:T0-EMIT:cc :}
   1 ENSURE
   TOP CONST? if FOLD1 exit then
   drop
   TOP MAT 0 T0-EMIT:CMP-LIT
   TOP REMOVE c CMP-FLAG ;

\ ---- opening and closing structures ------------------------------------------
: CS-PUSH ( n n -- ) {: kind:n loc:n :}
   CS-N @ {: k:n :}
   k 1+ CS-KIND-RESERVE  k 1+ CS-LOC-RESERVE  k LAY-ROOM
   kind k CS-KIND !  loc k CS-LOC !  -1 k CS-LAY-N !
   k 1+ CS-N ! ;

: CS-POP2 ( n n -- ) {: a:n b:n :}
   CS-N @ 0= if CONTROL-FAULT then
   CS-N @ 1- {: k:n :}
   k CS-KIND @ {: kind:n :}
   kind a <> kind b <> and if CONTROL-FAULT then
   k CS-N ! ;

: CS-POP ( n -- ) dup CS-POP2 ;

: CS-TOP-KIND ( -- n )
   CS-N @ 0= if CONTROL-FAULT then
   CS-N @ 1- CS-KIND @ ;

\ Valid only after a pop: the popped structure's slot sits at CS-N.
: CS-LOC@ ( -- n ) CS-N @ CS-LOC @ ;

: SCOPE-END ( -- ) CS-LOC@ L-N ! ;

: CC? ( -- bool ) PENDING @ 0<> ;

: CC-TAKE ( -- T0-EMIT:cc )
   PENDING @ 1- T0-EMIT:>CC
   0 PENDING ! ;

\ A spilling landing: the emptied virtual stack agrees with every arrival.
: LAND ( -- ) SPILL-ALL 0 DEAD ! ;

\ ---- joins -------------------------------------------------------------------
\ A layout is the virtual stack as registers alone: count n and n distinct
\ vregs, the top n cells of the data stack bottom first, the rest in memory.
\ `if` records the layout its skipped path carries, `begin` its loop head's,
\ `while` its exit's and `do` its level's; the paths that meet there
\ reconcile to the layout recorded first.

: HELD-BELOW? ( n -- bool ) {: k:n :}
   k VS-VAL @ {: v:n :}
   false k 0 ?do i REG? i VS-VAL @ v = and or loop ;

\ FREE is exactly the vregs no entry holds (ADOPT), less the flag `if` took off
\ the stack, so the entries and that flag, at most VREGS, leave ALLOC a free
\ vreg each time and it evicts nothing. LIT and MOV keep the condition codes
\ for a fused branch.
: SETTLE ( -- )
   VS-N @ 0 ?do
      i CONST? if i MAT drop else
         i HELD-BELOW? if
            i VREG@ {: s:T0-EMIT:vreg :}
            i RELABEL s T0-EMIT:MOV
         then
      then
   loop ;

: RECORD ( n -- ) {: k:n :}
   VS-N @ k CS-LAY-N !
   VS-N @ 0 ?do i VS-VAL @ k i LAY! loop ;

: SNAPSHOT ( n -- ) SETTLE RECORD ;

: LAY-COPY ( n n -- ) {: from:n to:n :}
   from CS-LAY-N @ to CS-LAY-N !
   T0-EMIT:VREGS 0 ?do from i LAY@ to i LAY! loop ;

\ FREE is rebuilt from the layout alone, so it is exactly the vregs no entry
\ holds, whatever the entries it replaces or RECONCILE's moves left allocated.
: ADOPT ( n -- ) {: k:n :}
   k CS-LAY-N @ VS-N !
   ALL-FREE FREE !
   VS-N @ 0 ?do
      k i LAY@ {: v:n :}
      K-REG v i ENTRY!
      1 v lshift invert FREE @ and FREE !
   loop
   0 DEAD ! ;

\ The entries match the layout's top cells.
: GOAL ( n n -- n ) {: k:n j:n :}
   k  k CS-LAY-N @ VS-N @ - j +  LAY@ ;

: AWAY? ( n n -- bool ) {: k:n j:n :}
   j REG? 0=  j VS-VAL @ k j GOAL <>  or ;

: AWAY-HOLDS? ( n n -- bool ) {: k:n v:n :}
   false VS-N @ 0 ?do i REG? i VS-VAL @ v = and k i AWAY? and or loop ;

: FIRST-AWAY ( n -- n ) {: k:n :}
   -1 VS-N @ 0 ?do dup 0< k i AWAY? and if drop i then loop ;

: READY ( n -- n ) {: k:n :}
   -1 VS-N @ 0 ?do
      dup 0< k i AWAY? and if
         k  k i GOAL  AWAY-HOLDS? 0= if drop i then
      then
   loop ;

: MOVE-TO ( n n -- ) {: k:n j:n :}
   k j GOAL {: t:n :}
   j REG? if t T0-EMIT:>VREG j VREG@ T0-EMIT:MOV
   else j VS-VAL @ t T0-EMIT:>VREG T0-EMIT:LIT then
   K-REG t j ENTRY! ;

: RENAME ( n n -- ) {: a:n b:n :}
   VS-N @ 0 ?do
      i REG? if
         i VS-VAL @ a = if b i VS-VAL ! else
            i VS-VAL @ b = if a i VS-VAL ! then
         then
      then
   loop ;

\ No entry is ready only when the waiting entries hold their goals and nothing
\ else, one vreg each, a permutation of them: an exchange then places entry j.
: CYCLE ( n n -- ) {: k:n j:n :}
   k j GOAL  j VS-VAL @ {: t:n v:n :}
   t T0-EMIT:>VREG v T0-EMIT:>VREG T0-EMIT:EXCHANGE
   t v RENAME ;

: SINK ( n -- ) {: d:n :}
   d 1 ?do d i - SPILL-ENTRY loop
   d PUSH-ENTRY
   d 0 ?do 0 REMOVE loop ;

: RAISE ( n n -- ) {: k:n d:n :}
   d 0 ?do
      d 1- i - {: e:n :}
      k e LAY@ T0-EMIT:>VREG {: r:T0-EMIT:vreg :}
      e 0= if r d T0-EMIT:POP else r e d - T0-EMIT:FILL then
   loop ;

\ An exact layout emits nothing; an unreachable fallthrough emits nothing and
\ takes the layout. Only the layout's own vregs are written, so a vreg outside
\ the layout keeps its value across it.
: RECONCILE ( n -- ) {: k:n :}
   DEAD @ if k ADOPT exit then
   VS-N @ k CS-LAY-N @ - {: d:n :}
   d 0 > if d SINK then
   begin k FIRST-AWAY 0 >= while
      k READY {: j:n :}
      j 0 >= if k j MOVE-TO else k k FIRST-AWAY CYCLE then
   repeat
   d 0 < if k d negate RAISE then
   k ADOPT ;

: DO-IF ( -- )
   CS-N @ {: k:n :}
   CS-IF L-N @ CS-PUSH
   CC? if
      CC-TAKE {: c:T0-EMIT:cc :}
      k SNAPSHOT
      c T0-EMIT:IF-CC, exit
   then
   TAKE {: f:T0-EMIT:vreg :}
   k SNAPSHOT
   f T0-EMIT:IF,
   f RELEASE ;

\ The first arm's end is the layout `then` reconciles to; the second arm
\ starts in the one `if` recorded. A dead first arm records none.
: DO-ELSE ( -- )
   CS-IF CS-POP
   SCOPE-END
   CS-N @ {: k:n :}
   k 1+ LAY-ROOM
   DEAD @ 0= {: live:bool :}
   live if k 1+ SNAPSHOT then
   T0-EMIT:ELSE,
   k ADOPT
   CS-ELSE L-N @ CS-PUSH
   live if k 1+ k LAY-COPY then ;

\ `then` reconciles to the layout its opener recorded: a `while`'s exit, or
\ an `if` or `else` path, and with none recorded every other arrival was
\ dead, so the virtual stack continues.
: DO-THEN ( -- )
   CS-TOP-KIND CS-WHILE = if CS-WHILE else CS-IF then
   CS-ELSE CS-POP2
   SCOPE-END
   CS-N @ {: k:n :}
   k CS-LAY-N @ 0 >= if k RECONCILE then
   T0-EMIT:THEN, ;

: DO-BEGIN ( -- )
   CS-N @ {: k:n :}
   CS-BEGIN L-N @ CS-PUSH  k SNAPSHOT
   T0-EMIT:BEGIN, ;

\ The exit's layout is recorded under the head's, which moves up a slot; the
\ body continues in the exit's.
: DO-WHILE ( -- )
   CS-BEGIN CS-POP
   CS-LOC@ {: loc:n :}
   CS-N @ {: k:n :}
   k CS-LAY-N @ {: head:n :}
   k 1+ LAY-ROOM  k k 1+ LAY-COPY
   CS-WHILE loc CS-PUSH
   CS-BEGIN loc CS-PUSH  head k 1+ CS-LAY-N !
   CC? if
      CC-TAKE {: c:T0-EMIT:cc :}
      k SNAPSHOT
      c T0-EMIT:WHILE-CC, exit
   then
   TAKE {: f:T0-EMIT:vreg :}
   k SNAPSHOT
   f T0-EMIT:WHILE,
   f RELEASE ;

: DO-REPEAT ( -- )
   CS-BEGIN CS-POP
   CS-N @ RECONCILE
   CS-WHILE CS-POP
   SCOPE-END
   T0-EMIT:REPEAT,
   CS-N @ ADOPT ;

: LAY-HOLDS? ( n T0-EMIT:vreg -- bool ) {: k:n r:T0-EMIT:vreg :}
   r T0-EMIT:VREG>N {: v:n :}
   false k CS-LAY-N @ 0 ?do k i LAY@ v = or loop ;

\ RECONCILE keeps the condition codes, so a pending compare's branch follows it.
\ A flag in a vreg the head holds is compared before RECONCILE can overwrite it,
\ and the loop goes back while it is zero, the compare's NE failing. A flag in a
\ vreg outside the head keeps its value across RECONCILE and is tested after.
: DO-UNTIL ( -- )
   CS-BEGIN CS-POP
   SCOPE-END
   CS-N @ {: k:n :}
   CC? if
      CC-TAKE {: c:T0-EMIT:cc :}
      k RECONCILE
      c T0-EMIT:UNTIL-CC, exit
   then
   TAKE {: f:T0-EMIT:vreg :}
   k f LAY-HOLDS? if
      f 0 T0-EMIT:CMP-LIT
      k RECONCILE
      T0-EMIT:CC-NE T0-EMIT:UNTIL-CC, exit
   then
   k RECONCILE
   f T0-EMIT:UNTIL, ;

: DO-AGAIN ( -- )
   CS-BEGIN CS-POP
   SCOPE-END
   CS-N @ RECONCILE
   T0-EMIT:AGAIN,
   1 DEAD ! ;

\ ---- loops -------------------------------------------------------------------
\ The index's home is the runtime frame LOOP-PUSH makes. LOOP-CLOSE lands the
\ `leave` exits and the `?do` entry on the frame's pop, so every way out
\ leaves through that pop in the level's layout.
\ A `do` level's layout L is the virtual stack under the start and limit, so it
\ holds at most VREGS - 2 entries: the closer's step and scratch vregs lie
\ outside it, and the emitter's loop words touch no other vreg.
variable DO-N                       \ the open levels

: LEVELS ( n -- ) DO-N @ > if CONTROL-FAULT then ;

\ The body, the back edge, the `?do` entry's skip and every `leave` meet in the
\ level's layout.
: OPEN-LEVEL ( bool -- ) {: entry:bool :}
   TAKE {: s:T0-EMIT:vreg :}
   TAKE {: l:T0-EMIT:vreg :}
   s l T0-EMIT:LOOP-PUSH
   CS-N @ {: k:n :}
   CS-DO L-N @ CS-PUSH  k SNAPSHOT
   s l entry T0-EMIT:LOOP-OPEN
   s RELEASE  l RELEASE
   DO-N @ 1+ DO-N ! ;

: DO-DO ( -- ) false OPEN-LEVEL ;

: DO-?DO ( -- ) true OPEN-LEVEL ;

: CLOSING ( -- ) CS-DO CS-POP SCOPE-END ;

: CLOSE-LEVEL ( -- )
   DO-N @ 1- DO-N !
   T0-EMIT:LOOP-CLOSE ;

: DO-LOOP ( -- )
   CLOSING
   CS-N @ RECONCILE
   ALLOC {: t:T0-EMIT:vreg :}
   t T0-EMIT:LOOP-NEXT
   t RELEASE
   CLOSE-LEVEL ;

: OUTSIDE ( n -- T0-EMIT:vreg ) {: k:n :}
   0 begin k over T0-EMIT:>VREG LAY-HOLDS? while 1+ repeat
   T0-EMIT:>VREG ;

: STEP-LAYOUT ( n -- ) {: k:n :}
   k 1+ LAY-ROOM  k k 1+ LAY-COPY
   k CS-LAY-N @ {: n:n :}
   k OUTSIDE T0-EMIT:VREG>N  k 1+ n LAY!
   n 1+ k 1+ CS-LAY-N ! ;

\ The step reconciles with the layout as a vreg above it, so the layout's moves
\ leave it whole and LOOP-STEP may clobber it.
: DO-+LOOP ( -- )
   CLOSING
   CS-N @ {: k:n :}
   k STEP-LAYOUT
   k 1+ RECONCILE
   TAKE {: s:T0-EMIT:vreg :}
   ALLOC {: t:T0-EMIT:vreg :}
   s t T0-EMIT:LOOP-STEP
   s RELEASE  t RELEASE
   CLOSE-LEVEL ;

: INNER-DO ( -- n )
   CS-N @ 1- begin dup CS-KIND @ CS-DO <> while 1- repeat ;

: DO-LEAVE ( -- )
   1 LEVELS
   INNER-DO RECONCILE
   T0-EMIT:LEAVE,
   1 DEAD ! ;

: DO-UNLOOP ( -- ) 1 LEVELS T0-EMIT:LOOP-POP ;

: DO-INDEX ( [ T0-EMIT:vreg -- ] n -- ) {: k:n :}
   k LEVELS
   ROOM ALLOC {: r:T0-EMIT:vreg :}
   r swap execute
   r PUSH-REG ;

\ ---- case --------------------------------------------------------------------
\ `of` compares the selector under the top with the top, which it drops; the
\ match drops the selector too, the mismatch branches past `endof` keeping it.
: OF-LIT ( -- )
   TOP VS-VAL @ {: v:n :}
   TOP REMOVE
   TOP MAT {: s:T0-EMIT:vreg :}
   SPILL-ALL
   s v T0-EMIT:OF-LIT ;

: OF-REG ( -- )
   TAKE {: v:T0-EMIT:vreg :}
   TOP MAT {: s:T0-EMIT:vreg :}
   SPILL-ALL
   s v T0-EMIT:OF, ;

: DO-CASE ( -- )
   T0-EMIT:CASE,
   CS-CASE L-N @ CS-PUSH ;

\ The case or MATCH whose arms the `endof`s on top close.
: ARMS-AT ( -- n )
   CS-N @ 1- begin dup 0 >= if dup CS-KIND @ CS-ENDOF = else false then while 1- repeat
   dup 0 < if CONTROL-FAULT then ;

: OF-TEST ( -- )
   2 ENSURE
   TOP IMM? if OF-LIT else OF-REG then
   DROP1
   CS-OF L-N @ CS-PUSH ;

: DO-OF ( -- )
   ARMS-AT CS-KIND @ CS-CASE <> if CONTROL-FAULT then
   OF-TEST ;

: DO-ENDOF ( -- )
   CS-OF CS-POP
   SCOPE-END
   LAND
   T0-EMIT:ENDOF,
   CS-ENDOF L-N @ CS-PUSH ;

\ The default arm drops the selector, and every `endof` lands after the drop.
: DO-ENDCASE ( -- )
   DROP1
   LAND
   begin CS-TOP-KIND CS-ENDOF = while CS-ENDOF CS-POP repeat
   CS-CASE CS-POP
   T0-EMIT:ENDCASE,
   SCOPE-END ;

\ ---- the rest of the keywords ------------------------------------------------
\ True once the body has left through a tail call: nothing follows it.
variable TAILED

: DO-EXIT ( -- ) SPILL-ALL T0-EMIT:RETURN 1 DEAD ! ;
: DO-RECURSE ( -- ) SPILL-ALL T0-EMIT:CALL-SELF ;

DYNAMIC-BUFFER RS-V T0-EMIT:vreg     \ reserved to T0-EMIT:VREGS by RESET: one
                                     \ return-stack move's vregs, the deepest
                                     \ slot first

: RS-PART ( n -- n ) T0-EMIT:VREGS min ;

\ The top cell goes into the deepest slot.
: TOR-PART ( n -- ) {: c:n :}
   c 0 ?do TAKE i RS-V ! loop
   0 RS-V c T0-EMIT:TOR
   c 0 ?do i RS-V @ RELEASE loop ;

\ c free vregs in RS-V, with room for an entry each.
: RS-ALLOC ( n -- ) {: c:n :}
   begin VS-N @ c + T0-EMIT:VREGS > while EVICT repeat
   c 0 ?do ALLOC i RS-V ! loop ;

\ RS-V's c vregs pushed in the order `r>` leaves cells: the top slot's deepest.
: RS-PUSH ( n -- ) {: c:n :}
   c 0 ?do c 1- i - RS-V @ PUSH-REG loop ;

: RFROM-PART ( n -- ) {: c:n :}
   c RS-ALLOC  0 RS-V c T0-EMIT:RFROM  c RS-PUSH ;

\ The c cells k under the top, copied as RFROM-PART would pop them.
: RFETCH-PART ( n n -- ) {: c:n k:n :}
   c RS-ALLOC  0 RS-V c k T0-EMIT:RFETCH  c RS-PUSH ;

: TOR-CELLS ( n -- )
   begin dup 0 > while dup RS-PART dup TOR-PART - repeat drop ;

: RFROM-CELLS ( n -- )
   begin dup 0 > while dup RS-PART dup RFROM-PART - repeat drop ;

: RFETCH-CELLS ( n -- ) {: n:n :}
   0 begin dup n < while dup n swap - RS-PART  2dup swap RFETCH-PART  + repeat drop ;

: PAIR-CELLS ( -- n ) 0 WS @ 1 WS @ + ;

\ `2>r` is `swap >r >r`, `2r>` is `r> r> swap`, `2r@` is `2r> 2dup 2>r`: over
\ one-cell operands one move, the deeper operand in the deeper slot, as the
\ primitives order them (habu1.f B2TOR).
: 2TOR ( -- ) SWAP-OP PAIR-CELLS TOR-CELLS ;
: 2RFROM ( -- ) PAIR-CELLS RFROM-CELLS  WS-SWAP SWAP-OP WS-SWAP ;
: 2RFETCH ( -- ) PAIR-CELLS RFETCH-CELLS  WS-SWAP SWAP-OP WS-SWAP ;

\ The code record rec offers a compiled reference, or 0: a retired record's
\ code nothing reaches (src/compiler/native/dict.f REC-CODE).
: REC-START ( ptr n -- n ) {: rec:ptr :}
   rec XREF-RETIRED? if 0 exit then
   rec XREF-START ;

\ An operand bound to no record, or one NDICT:TICK-REFUSED? refuses, is undefined
\ (MISSING), as tier 1's tick refuses it, even in an unchecked body (THE EVENTS).
: DO-TICK ( n -- ) {: r:n :}
   NEXT$ {: a:ptr u:n :}
   r 0< if -1 MISSING !  a u UNSUPPORTED then
   r XREF-REC {: rec:ptr :}
   rec NDICT:TICK-REFUSED? if -1 MISSING !  a u UNSUPPORTED then
   rec REC-START {: xt:n :}
   xt 0= if a u UNSUPPORTED then
   ROOM ALLOC {: r:T0-EMIT:vreg :}
   xt r T0-EMIT:ADDR-CODE
   r PUSH-REG ;

\ Character row k: its operand's first byte, the row's a0: `[char] é` is the
\ 195 that begins its UTF-8.
: DO-CHAR ( n -- ) ROW-A0 @ PUSH-CONST ;

\ A call spills first; the last token of a body outside every structure leaves
\ through the callee when the callee lies in the code region, as tier 1's
\ (src/compiler/native/compiler.f LINKAGE): publication refuses a tail branch
\ out of it. loc is the calling token's offset in the text.
: DO-CALL ( n n -- ) {: to:n loc:n :}
   SPILL-ALL
   CS-N @ 0= AT-END? and to NPUB:IN-REGION? and if
      to loc T0-EMIT:TAIL
      1 TAILED ! exit
   then
   to loc T0-EMIT:CALL ;

: MOVE-OP ( ptr u8 n ptr u8 n -- ) {: a:ptr k:n outs:ptr u:n :}
   a k WIDTHS  outs u MOVE-WIDE ;

\ ---- whole-width memory ------------------------------------------------------
: CELLS0 ( ptr u8 -- n ) {: a:ptr :} a 1 WIDTHS 0 WS @ ;

: ADDR+ ( n -- ) {: k:n :}
   k 0= if exit then
   k cells PUSH-CONST [: + ;] T0-EMIT:OP-ADD true ALU2 ;

\ Push data address v, whose row files origin o.
: PUSH-ADDR ( n n -- ) {: v:n o:n :}
   ROOM ALLOC {: r:T0-EMIT:vreg :}
   v o r T0-EMIT:ADDR-DATA
   r PUSH-REG ;

\ A fetch the checker filed a layout fact for calls NFETCH-CHECK:TAGS with a
\ copy of the address and the fact's descriptor before any cell is loaded, as
\ tier 1's VALIDATE-FETCH (src/compiler/native/elaborate.f), whose comment says
\ why the descriptor's address literal is the compiler's own trusted producer of
\ a frozen value. NFETCH:CAPTURE keys each fact by its offset in the whole text,
\ the clause's base added, so the key is the token's offset with no cut taken.
: VALIDATE ( ptr u8 n -- ) {: a:ptr w:n :}
   a TXT$ - {: at:n :}
   at NFETCH:AT {: address:n bytes:n width:n origin:n :}
   bytes 0= if exit then
   width w <> if E-T0-BUNDLE throw then
   s" NFETCH-CHECK:TAGS" NDICT:CALL-TARGET {: entry:n :}
   entry 0= if E-HIR-UNMODELED throw then
   0 DEEP
   address origin PUSH-ADDR
   SPILL-ALL  entry at T0-EMIT:CALL ;

: DO-FETCH ( ptr u8 -- ) {: a:ptr :}
   a CELLS0 {: w:n :}
   w 1 < if E-T0-BUNDLE throw then
   a w VALIDATE
   w 1 = if T0-EMIT:CELL-W FETCH exit then
   w 0 ?do i DEEP i ADDR+ T0-EMIT:CELL-W FETCH loop
   w 1 NIP-CELLS ;

\ The wide store's calls never leave as a tail: each stores a copy of one cell,
\ so the w cells and the address are still on the stack after the last.
: DO-STORE ( ptr u8 n -- ) {: a:ptr to:n :}
   a CELLS0 {: w:n :}
   w 1 < if E-T0-BUNDLE throw then
   a TXT$ - {: loc:n :}
   w 1 = if to loc DO-CALL exit then
   w 0 ?do
      w i - DEEP  1 DEEP  i ADDR+
      SPILL-ALL  to loc T0-EMIT:CALL
   loop
   w 1+ negate T0-EMIT:DSP+ ;

\ ---- strings -----------------------------------------------------------------
\ A string is one row whose span is the payload; the checker reports its body,
\ decoded for an escaped literal (src/core/checker.f CHECKER-TAPE:STRING), and
\ the tape copies it. Bytes are interned (NSTR:INTERN-SOURCE), so equal bodies
\ share an address, and the address row files the origin interning answers. A
\ counted string past COUNTED-MAX bytes is refused with tier 1's message and
\ exit code 76.
255 constant COUNTED-MAX            \ a counted string's length is one byte

DYNAMIC-BUFFER STR-BUF u8           \ a decoded or counted body before interning

: STRING-KIND? ( n -- bool ) {: kind:n :}
   kind CHECKER-TAPE:K-STRING =  kind CHECKER-TAPE:K-COUNTED-STRING = or
   kind CHECKER-TAPE:K-PRINTED-STRING = or ;

\ A counted string of u bytes refuses as tier 1's COUNTED-LENGTH-CK does.
: TOO-LONG ( n -- )
   COUNTED-MAX <= if exit then
   2 S\" hb: counted string too long (max 255)\n" write drop
   76 throw ;                       \ the exit code src/habu/outer.f RC-TOO-LONG defines

\ String row k's interned address, its source origin and its length.
: INTERN-BODY ( n bool -- n n n ) {: k:n counted:bool :}
   TXT$ k ROW-OFF @ + {: s:ptr :}
   k ROW-AT @ {: body:n :}
   k ROW-U @ {: u:n :}
   counted if u TOO-LONG then
   body 0< u 0= or counted 0= and if s u NSTR:INTERN-SOURCE u exit then
   counted if 1 else 0 then {: at:n :}
   at u + STR-BUF-RESERVE
   body 0< if s at STR-BUF u BYTE-COPY else body BODIES at STR-BUF u BYTE-COPY then
   counted if u 0 STR-BUF c! then
   0 STR-BUF at u + NSTR:INTERN-SOURCE u ;

\ `."` and `is` call the engine's `type` and `xt!` whatever the source's
\ package scope.
CAST: XT>N ( xt -- n )

\ The payload's offset is the call's for `."`, as tier 1's (src/core/checker.f
\ SPAY-B).
: DO-STRING ( n -- ) {: k:n :}
   k ROW-KIND @ {: kind:n :}
   kind CHECKER-TAPE:K-COUNTED-STRING = {: counted:bool :}
   k counted INTERN-BODY {: v:n o:n u:n :}
   v o PUSH-ADDR
   counted if exit then
   u PUSH-CONST
   kind CHECKER-TAPE:K-PRINTED-STRING = if ['] type XT>N k ROW-OFF @ DO-CALL then ;

\ ---- is ----------------------------------------------------------------------
\ `is W` pushes W's cell (NDICT:REC-DEFER-CELL) and calls `xt!`, as tier 1's
\ elaborate.f DO-IS; a W that is missing or no deferred word is E-T0-DEFER.

: DO-IS ( n n -- ) {: r:n loc:n :}
   ORD @ ROWS @ >= if ORD @ 1- ROW$ E-T0-DEFER REFUSE then
   NEXT$ {: a:ptr u:n :}             \ the operand
   r 0< if a u E-T0-DEFER REFUSE then
   r XREF-REC NDICT:REC-DEFER-CELL {: cell:n :}
   cell 0= if a u E-T0-DEFER REFUSE then
   cell r PUSH-ADDR
   ['] xt! XT>N loc DO-CALL ;

\ ---- quotations --------------------------------------------------------------
\ A quotation is a unit of its own in the same emission (src/habu/t0-emit.f
\ THE UNITS). At `[:` the body pushes the unit's address and skips the rows
\ through the matching `;]`; once the definition is compiled each quotation is
\ compiled from its first row (QORD), in the order the `[:` tokens were met,
\ nested ones after their parent. Each unit starts with an empty virtual
\ stack, no structure, loop level or local, as the checker reads a quotation.
\ A local group inside a quotation, or a read of an enclosing local, never
\ reaches T0 as one: the checker refuses it in a checked body
\ (E-BAD-LOCAL-SHAPE), and in a TRUSTED: body its tokens bind no record,
\ refused E-T0-UNSUPPORTED at the first.
variable NQ                         \ units met so far, the definition's included
DYNAMIC-BUFFER QORD n               \ each quotation's first ordinal
variable NESTED                     \ the `[:` rows skipped inside quotations so far

: SKIP-ROW ( n -- n ) {: k:n :}
   k ROWS @ >= if NDEFINITION:CLOSE$ E-T0-CONTROL REFUSE then
   k ROW-KIND @ CHECKER-TAPE:K-UNREACHED = if k ROW$ E-T0-CONTROL REFUSE then
   k ROW-KIND @ CHECKER-TAPE:K-CONTROL <> if 0 exit then
   k ROW-A0 @ CF-QUOT-OPEN = if 1 NESTED +! 1 exit then
   k QUOT-END? if -1 exit then
   0 ;

: SKIP-QUOT ( -- )
   1 begin dup 0 > while ORD @ SKIP-ROW +  1 ORD +! repeat drop ;

: PUSH-UNIT ( n -- ) {: k:n :}
   ROOM ALLOC {: r:T0-EMIT:vreg :}
   k r T0-EMIT:ADDR-QUOT
   r PUSH-REG ;

: DO-QUOT ( -- )
   NQ @ {: k:n :}
   k 1+ QORD-RESERVE
   ORD @ k QORD !
   k 1+ NQ !
   SKIP-QUOT
   k PUSH-UNIT ;

\ ---- the does> split ---------------------------------------------------------
\ A `does>` definer is one text split at M-DOES, just past its `does> `, which
\ NDEFINITION:CHECK-DOES-HEAD checks. The parent is unit 0 up to the `does>`
\ token, where it calls `does-patch` with the clause's address (0 for an empty
\ clause) and the clause's interned signature, then returns. The clause is the
\ unit after every quotation of the parent text, nested ones included, as tier 1
\ numbers it; its own quotations follow it, and an empty clause's unit is still
\ built. The clause binds locals of its own, whose cells its own scan measured.
variable EMPTY-CLAUSE               \ whether no row follows the split: the clause is empty
variable CLAUSE-ORD                 \ the checker's ordinal of the clause's first token

: CLAUSE-EMPTY? ( -- bool ) CLAUSE-ORD @ ROWS @ >= ;

\ As tier 1's STAGE-DOES-PATCH (src/compiler/native/elaborate.f).
: DOES-TAIL ( -- )
   NQ @ NESTED @ + CLAUSE !
   EMPTY-CLAUSE @ if 0 PUSH-CONST else CLAUSE @ PUSH-UNIT then
   NDEFINITION:M-DOES-SIG @ NDEFINITION:M-DOES-SIG-U @ NSTR:INTERN-SOURCE PUSH-ADDR
   NDEFINITION:M-DOES-SIG-U @ PUSH-CONST
   SPILL-ALL
   NDICT:DOES-PATCH-ENTRY CUT @ NDEFINITION:DOES-SPAN - T0-EMIT:CALL ;

\ ---- MATCH and construct -----------------------------------------------------
\ `MATCH fam` reads its value as the cells the checker proved at the family
\ token, tag on top, and opens a case over the tag; each arm drops the tag and
\ the pads proved at its `of`. `;MATCH` is the default arm: an unmatched tag
\ calls the trap routine with the family's bad-tag message
\ (src/compiler/native/trap.f), as tier 1's MATCH-TRAP. `construct fam v` pushes
\ the variant's pads as zeros, then its tag. A call first pushes, as zeros, the
\ pads the checker proved the call's instantiation adds (NDICT:CON-PADS):
\ OPTION:NONE at option<pt>. A form whose facts are missing or disagree, as
\ tier 1's MATCH-SCAN refuses them, is E-T0-MATCH: MATCH-CK names the token
\ being compiled, a family or variant that resolves nothing or a MATCH with no
\ cells proved under its tag; DO-ARM names the token in place of a missing `of`.
\ The facts are the checker's, filed under the ordinal of the token that holds
\ them, as tier 1 reads them (src/compiler/native/elaborate.f MATCH-SCAN).
: MATCH-CK ( bool -- ) 0= if ORD @ 1- ROW$ E-T0-MATCH REFUSE then ;

\ `;MATCH` closes MATCH k: a tag no arm matched runs into the trap on the last
\ arm's mismatch edge, as tier 1's MATCH-TRAP, and every `endof` lands after
\ it. loc is the `;MATCH` token's offset in the text.
: CLOSE-MATCH ( n n -- ) {: loc:n k:n :}
   CS-TOP-KIND CS-MATCH <> MATCH-CK
   k CS-FAM @ NFAM:NAME$ NTRAP:BAD-TAG {: a:ptr u:n rc:n :}
   a u NSTR:INTERN-SOURCE PUSH-ADDR
   u PUSH-CONST  rc PUSH-CONST
   NTRAP:ROUTINE loc DO-CALL
   begin CS-TOP-KIND CS-ENDOF = while CS-ENDOF CS-POP repeat
   CS-MATCH CS-POP
   T0-EMIT:ENDCASE,
   SCOPE-END ;

\ The mismatch keeps the value for the next arm.
: ARM-OF ( n -- ) {: at:n :}
   ARMS-AT {: k:n :}
   k CS-KIND @ CS-MATCH = MATCH-CK
   at NDICT:MATCH-CELLS {: pads:n :}
   pads NDICT:MATCH-NONE <> MATCH-CK
   k CS-WID @ 1- pads - {: pay:n :}
   pay 0 >= MATCH-CK
   at NDICT:MATCH-PAYLOAD drop pay = MATCH-CK
   OF-TEST
   pads 0 ?do DROP1 loop ;

: DO-MATCH ( n n -- ) {: fam:n at:n :}
   fam 0 >= MATCH-CK
   at NDICT:MATCH-CELLS {: w:n :}
   w NDICT:MATCH-NONE <> MATCH-CK
   w fam NFAM:WIDTH >= MATCH-CK
   T0-EMIT:CASE,
   CS-N @ {: k:n :}
   CS-MATCH L-N @ CS-PUSH
   k 1+ CS-FAM-RESERVE  k 1+ CS-WID-RESERVE
   fam k CS-FAM !  w k CS-WID ! ;

\ A variant's tag; the checker files K-MATCH-OF only for an `of`, which a body
\ ending here lacks.
: DO-ARM ( n -- ) {: tag:n :}
   tag 0 >= MATCH-CK
   ORD @ ROWS @ >= if NDEFINITION:CLOSE$ E-T0-MATCH REFUSE then
   ORD @ ROW-KIND @ CHECKER-TAPE:K-MATCH-OF <> if ORD @ ROW$ E-T0-MATCH REFUSE then
   tag PUSH-CONST ;

\ As tier 1's DO-MAKE-BUNDLE; the payload is already on the stack.
: DO-CONSTRUCT ( n n n -- ) {: tag:n pads:n at:n :}
   tag 0 >= MATCH-CK
   at NDICT:CALL-CELLS drop 0 >= MATCH-CK
   pads at NDICT:CON-PADS +  0 ?do 0 PUSH-CONST loop
   tag PUSH-CONST ;

\ ---- the inlined operations --------------------------------------------------
\ THE INLINED OPERATIONS. Each operation T0 inlines is one of the engine's
\ seeded primitives, a record below the source prefix's first
\ (CORE-PREFIX:FIRST-RECORD), and OP-OF maps such a record's dictionary index to
\ the operation's code in INLINE, 0 for none. OPS-MAP fills it once, when T0
\ loads, by each operation's name; a record at or above the boundary is never
\ an operation, so a package's own word of that name is a call. OP-OF is
\ dictionary storage, so a saved image keeps it filled.
CORE-PREFIX:FIRST-RECORD constant OP-N   \ the records OP-OF holds
OP-N TYPED-BUFFER OP-OF n
variable OP-HITS                     \ the seeded records OP found by the name
variable OP-REC                      \ the last of them

\ End the load: operation a u names no single seeded record.
: OP-DIE ( ptr u8 n -- )
   {: a:ptr u:n :}
   s" : T0: an inlined operation names no single seeded record" {: m:ptr k:n :}
   u k + STR-BUF-RESERVE
   a 0 STR-BUF u BYTE-COPY
   m u STR-BUF k BYTE-COPY
   0 STR-BUF u k + 76 die ;

: OP ( ptr u8 n n -- )
   {: a:ptr u:n code:n :}
   0 OP-HITS !
   OP-N 0 ?do
      i XREF-REC a u XREF-MATCH? if 1 OP-HITS +!  i OP-REC ! then
   loop
   OP-HITS @ 1 <> if a u OP-DIE then
   code OP-REC @ OP-OF ! ;

: OPS-MAP ( -- )
   s" +" 1 OP  s" -" 2 OP  s" *" 3 OP  s" /" 4 OP  s" @" 5 OP
   s" !" 6 OP  s" =" 7 OP  s" <" 8 OP  s" >" 9 OP  s" or" 10 OP
   s" 1+" 11 OP  s" 1-" 12 OP  s" c@" 13 OP  s" 0=" 14 OP
   s" 0<" 15 OP  s" <>" 16 OP  s" <=" 17 OP  s" >=" 18 OP
   s" dup" 19 OP  s" rot" 20 OP  s" nip" 21 OP  s" 2>r" 22 OP
   s" 2r>" 23 OP  s" 2r@" 24 OP  s" and" 25 OP  s" mod" 26 OP
   s" xor" 27 OP  s" drop" 28 OP  s" swap" 29 OP  s" over" 30 OP
   s" tuck" 31 OP  s" -rot" 32 OP  s" 2dup" 33 OP  s" 2drop" 34 OP
   s" 2swap" 35 OP  s" 2over" 36 OP  s" cells" 37 OP  s" cell+" 38 OP
   s" negate" 39 OP  s" invert" 40 OP  s" lshift" 41 OP
   s" rshift" 42 OP  s" ptr-field" 43 OP ;
OPS-MAP

: OP-CODE ( n -- n )
   {: r:n :}
   r OP-N >= if 0 exit then
   r OP-OF @ ;

: CELL1 ( ptr u8 -- )
   {: a:ptr :}
   a CELLS0 1 <> if E-T0-BUNDLE throw then ;

\ Operation code at a, whose primitive's entry is to: false when it is not
\ inlined after all (DIV2), so the name is a call.
: INLINE ( ptr u8 n n -- bool )
   {: a:ptr code:n to:n :}
   code case
      1 of a CELL1 [: + ;] T0-EMIT:OP-ADD true ALU2 true endof
      2 of a CELL1 [: - ;] T0-EMIT:OP-SUB false ALU2 true endof
      3 of a CELL1 MUL2 true endof
      4 of a CELL1 [: / ;] [: T0-EMIT:DIV-LIT ;] DIV2 endof
      5 of a DO-FETCH true endof
      6 of a to DO-STORE true endof
      7 of a CELL1 [: = bool>n ;] T0-EMIT:CC-EQ CMP2 true endof
      8 of a CELL1 [: < bool>n ;] T0-EMIT:CC-LT CMP2 true endof
      9 of a CELL1 [: > bool>n ;] T0-EMIT:CC-GT CMP2 true endof
      10 of a CELL1 [: or ;] T0-EMIT:OP-OR true ALU2 true endof
      11 of a CELL1 1 PUSH-CONST [: + ;] T0-EMIT:OP-ADD true ALU2 true endof
      12 of a CELL1 1 PUSH-CONST [: - ;] T0-EMIT:OP-SUB false ALU2 true endof
      13 of a CELL1 T0-EMIT:BYTE-W FETCH true endof
      14 of a CELL1 [: 0= bool>n ;] T0-EMIT:CC-EQ CMP0 true endof
      15 of a CELL1 [: 0< bool>n ;] T0-EMIT:CC-LT CMP0 true endof
      16 of a CELL1 [: <> bool>n ;] T0-EMIT:CC-NE CMP2 true endof
      17 of a CELL1 [: <= bool>n ;] T0-EMIT:CC-LE CMP2 true endof
      18 of a CELL1 [: >= bool>n ;] T0-EMIT:CC-GE CMP2 true endof
      19 of [: DUP1 ;] a 1 s" 00" SHUFFLE true endof
      20 of [: ROT1 ;] a 3 s" 102" SHUFFLE true endof
      21 of [: NIP1 ;] a 2 s" 0" SHUFFLE true endof
      22 of a 2 WIDTHS 2TOR true endof
      23 of a 2 WIDTHS 2RFROM true endof
      24 of a 2 WIDTHS 2RFETCH true endof
      25 of a CELL1 [: and ;] T0-EMIT:OP-AND true ALU2 true endof
      26 of a CELL1 [: mod ;] [: T0-EMIT:MOD-LIT ;] DIV2 endof
      27 of a CELL1 [: xor ;] T0-EMIT:OP-XOR true ALU2 true endof
      28 of [: DROP1 ;] a 1 s" " SHUFFLE true endof
      29 of [: SWAP1 ;] a 2 s" 01" SHUFFLE true endof
      30 of [: OVER1 ;] a 2 s" 101" SHUFFLE true endof
      31 of [: TUCK1 ;] a 2 s" 010" SHUFFLE true endof
      32 of a 3 s" 021" MOVE-OP true endof
      33 of a 2 s" 1010" MOVE-OP true endof
      34 of a 2 s" " MOVE-OP true endof
      35 of a 4 s" 1032" MOVE-OP true endof
      36 of a 4 s" 321032" MOVE-OP true endof
      37 of a CELL1 CELLS-OP true endof
      38 of a CELL1 CELL PUSH-CONST [: + ;] T0-EMIT:OP-ADD true ALU2 true endof
      39 of a CELL1 [: negate ;] [: T0-EMIT:NEG ;] UNARY true endof
      40 of a CELL1 [: invert ;] [: T0-EMIT:NOT ;] UNARY true endof
      41 of a CELL1 [: lshift ;] T0-EMIT:SH-LEFT SHIFT2 true endof
      42 of a CELL1 [: rshift ;] T0-EMIT:SH-RIGHT SHIFT2 true endof
      43 of a CELL1 CELLS-OP [: + ;] T0-EMIT:OP-ADD true ALU2 true endof
      E-T0-UNSUPPORTED throw
   endcase ;

\ A K-CONTROL row at a: code is the marker's place in the checker's list
\ (src/core/checker.f CF-TOK?, then RS-KEYWORD's `>r` `r>` `r@`). The `;]`
\ ends its unit before it is compiled (BODY), and a stray `;MATCH` (9) never
\ certifies.
: DO-CONTROL ( ptr u8 n -- ) {: a:ptr code:n :}
   code case
      CF-QUOT-OPEN of DO-QUOT endof
      CF-IF of DO-IF endof
      CF-ELSE of DO-ELSE endof
      CF-THEN of DO-THEN endof
      CF-CASE of DO-CASE endof
      CF-OF of DO-OF endof
      CF-ENDOF of DO-ENDOF endof        \ a case's `endof` or a MATCH's
      CF-ENDCASE of DO-ENDCASE endof
      CF-BEGIN of DO-BEGIN endof
      CF-UNTIL of DO-UNTIL endof
      CF-AGAIN of DO-AGAIN endof
      CF-WHILE of DO-WHILE endof
      CF-REPEAT of DO-REPEAT endof
      CF-DO of DO-DO endof
      CF-?DO of DO-?DO endof
      CF-LOOP of DO-LOOP endof
      CF-+LOOP of DO-+LOOP endof
      CF-I of [: T0-EMIT:LOOP-I ;] 1 DO-INDEX endof
      CF-J of [: T0-EMIT:LOOP-J ;] 2 DO-INDEX endof
      CF-EXIT of DO-EXIT endof
      CF-LEAVE of DO-LEAVE endof
      CF-UNLOOP of DO-UNLOOP endof
      CF-RECURSE of DO-RECURSE endof
      CF-TOR of a CELLS0 TOR-CELLS endof
      CF-RFROM of a CELLS0 RFROM-CELLS endof
      CF-RFETCH of a CELLS0 RFETCH-CELLS endof
      CONTROL-FAULT
   endcase ;

\ ---- one token ---------------------------------------------------------------
\ THE EVENTS. A token is what the checker filed it as (CHECKER-TAPE's kinds). A
\ name inlines only when its bound record is the engine's seeded primitive of
\ the operation (THE INLINED OPERATIONS); a package's own word named like a
\ primitive is a call. A word whose definer fixed a value pushes it
\ (WORD-CALL). Anything else is E-T0-UNSUPPORTED (UNSUPPORTED$). A name compiled
\ code cannot reach (MISSING) is refused as undefined, as the JIT's lookup
\ refuses it, even in an unchecked body whose scan did not refuse it.
\ A token no path reaches (K-UNREACHED) is E-T0-CONTROL, as tier 1's
\ E-NELAB-CTRL.
\
\ T0 passes over a parse-neutral immediate as tier 1 does
\ (src/compiler/native/elaborate.f WALK-SKIP?) and refuses any other.
: IMM-TOKEN ( ptr u8 n -- ) {: a:ptr u:n :}
   a u CHECKER-OWNER:PARSE-IMM? if exit then
   a u E-UNMODELED-IMMEDIATE REFUSE ;

\ A fixed word is its stamped value, as tier 1's RESOLVE-SITE folds it; r files a
\ DATA address so publication relocates it. A created word made before the AOT
\ window opened stays a call: the capture refuses its address. User-space code
\ addresses and 0 lie below 2^63, so the signed >= agrees with the JIT's unsigned
\ compare (habu2.f EM-COMPILE-CALL).
: WORD-CALL ( n n n n -- )
   {: to:n kind:n r:n loc:n :}
   kind DKIND:VAL = if to NDICT:FIXED-VAL NDICT:BOUND-VALUE PUSH-CONST exit then
   kind DKIND:ADDR = to data-base AOT-CELLS:B0-CELL + @ >= and if
      to NDICT:FIXED-ADDR NDICT:BOUND-VALUE  r PUSH-ADDR exit
   then
   ORD @ 1- {: at:n :}
   at NDICT:CALL-EXTRAS 0 ?do  at i NDICT:CALL-EXTRA PUSH-CONST  loop
   to loc DO-CALL ;

: NAME-TOKEN ( ptr u8 n n n -- )
   {: a:ptr u:n r:n loc:n :}
   r 0< if -1 MISSING !  a u UNSUPPORTED then
   r XREF-REC {: rec:ptr :}
   rec REC-START {: to:n :}
   r OP-CODE {: code:n :}
   code 0<> if a code to INLINE if exit then then
   rec XREF-FLAGS {: flags:n :}
   flags DNAME-IMM and 0<> if a u IMM-TOKEN exit then
   flags DNAME-INT and 0<> if
      flags NDICT:INT-CALL? 0= if -1 MISSING !  a u UNSUPPORTED then
   then
   to 0= if a u UNSUPPORTED then
   to flags DKIND:MASK and r loc WORD-CALL ;

\ The row at the cursor, which the walk then passes, by the event the checker
\ filed for it (THE EVENTS). A string or character literal's span is its
\ payload; a name lowers from its record.
: COMPILE-TOKEN ( -- )
   ORD @ {: k:n :}
   k ROW-KIND @ {: event:n :}
   k ROW-A0 @ {: a0:n :}
   NEXT$ {: a:ptr u:n :}
   a TXT$ - {: loc:n :}
   event STRING-KIND? if k DO-STRING exit then
   event case
      CHECKER-TAPE:K-NAME of a u a0 loc NAME-TOKEN endof
      CHECKER-TAPE:K-INT of a0 PUSH-CONST endof
      CHECKER-TAPE:K-REAL of a0 PUSH-CONST endof
      CHECKER-TAPE:K-CHAR of k DO-CHAR endof
      CHECKER-TAPE:K-CONTROL of a a0 DO-CONTROL endof
      CHECKER-TAPE:K-LOCAL-DECL of a0 loc DECLARE endof
      CHECKER-TAPE:K-LOCAL-CLOSE of CLOSE-GROUP endof
      CHECKER-TAPE:K-LOCAL-REF of a0 DO-LOCAL endof
      CHECKER-TAPE:K-OPERAND of endof
      CHECKER-TAPE:K-UNREACHED of a u E-T0-CONTROL REFUSE endof
      CHECKER-TAPE:K-TICK of a0 DO-TICK endof
      CHECKER-TAPE:K-IS of a0 loc DO-IS endof
      CHECKER-TAPE:K-MATCH of a0 k DO-MATCH endof
      CHECKER-TAPE:K-MATCH-ARM of a0 DO-ARM endof
      CHECKER-TAPE:K-MATCH-OF of k ARM-OF endof
      CHECKER-TAPE:K-MATCH-END of loc ARMS-AT CLOSE-MATCH endof
      CHECKER-TAPE:K-CONSTRUCT of a0 k ROW-A1 @ k DO-CONSTRUCT endof
      a u UNSUPPORTED
   endcase ;

\ ---- one unit ----------------------------------------------------------------
\ Compile rows to the end of the unit: whether a `;]` ended it rather than
\ the end of the tape or the split's `does>`.
: BODY ( -- bool )
   begin
      ORD @ ROWS @ >= if false exit then
      ORD @ SPLIT @ = if false exit then
      ORD @ QUOT-END? if 1 ORD +! true exit then
      COMPILE-TOKEN
   again ;

: RESET ( -- )
   T0-EMIT:VREGS VS-KIND-RESERVE  T0-EMIT:VREGS VS-VAL-RESERVE  T0-EMIT:VREGS RS-V-RESERVE
   0 VS-N !  ALL-FREE FREE !  0 CS-N !  0 DO-N !  0 L-N !  0 TAILED !
   0 DEAD !  0 PENDING !  0 G-CELLS ! ;

\ The token that ended a body short of a `;]`: the split's `does>`, or `;` past
\ the last row.
: END$ ( -- ptr u8 n )
   ORD @ ROWS @ >= if NDEFINITION:CLOSE$ exit then
   ORD @ ROW$ ;

: FINISH ( ptr u8 n -- ) {: a:ptr u:n :}
   CS-N @ 0<> if a u E-T0-CONTROL REFUSE then
   TAILED @ 0= if SPILL-ALL T0-EMIT:RETURN then
   T0-EMIT:CLOSE ;

: QUOT ( n -- ) {: k:n :}
   k QORD @ ORD !
   RESET
   k T0-EMIT:QUOT-OPEN
   BODY 0= if END$ E-T0-CONTROL REFUSE then
   ORD @ 1- ROW$ FINISH ;

: BINDS-CK ( n -- ) {: base:n :}
   BINDS @ 0= if exit then
   BINDS @  base SPLIT-BINDS nip  <> if E-T0-BINDS throw then ;

\ Quotation units from n until every unit met is compiled: the next unit.
: UNITS ( n -- n )
   begin dup NQ @ < while dup QUOT 1+ repeat ;

\ An empty clause's unit is still built, with no address bound to it: the
\ parent pushed 0.
: DO-CLAUSE ( n -- ) {: k:n :}
   CLAUSE-ORD @ ORD !  k 1+ NQ !  RESET  0 BINDS !
   EMPTY-CLAUSE @ if T0-EMIT:UNIT-OPEN else k T0-EMIT:QUOT-OPEN then
   BODY if CONTROL-FAULT then
   END$ FINISH
   CUT @ BINDS-CK ;

\ The name is row 0, so the body starts at row 1.
: WALK ( -- )
   NDEFINITION:SRC$ drop TXT !
   NDEFINITION:M-DOES @ {: cut:n :}
   1 NQ !  0 BINDS !
   cut CUT !  -1 CLAUSE !  0 NESTED !
   cut 0<> if CLAUSE-EMPTY? else false then EMPTY-CLAUSE !
   1 ORD !
   RESET
   T0-EMIT:OPEN
   BODY if CONTROL-FAULT then
   cut 0<> if
      ORD @ SPLIT @ <> if SPLIT @ ROW$ E-T0-CONTROL REFUSE then
      DOES-TAIL
   then
   END$ FINISH
   0 BINDS-CK
   1 UNITS
   cut 0= if drop exit then
   CLAUSE @ <> if SPLIT @ ROW$ E-T0-CONTROL REFUSE then
   CLAUSE @ DO-CLAUSE
   CLAUSE @ 1+ UNITS drop ;

public

\ The token this compile's refusal named, for a code REFUSED$ answers with it;
\ empty until one does.
: UNSUPPORTED$ ( -- ptr u8 n ) BAD @ BAD-LEN @ ;

using NDEFINITION
private

\ ---- the runtime compiler ----------------------------------------------------
\ T0:INSTALL stores T0:COMPILE in the cell the tier 0 `:` dispatches through
\ (T0:DISPATCH!), with no fallback to the JIT, and stores NDEFINITION's
\ CALLBACK-STUB! and CALLBACK-STUB@ in the cells CALLBACK:
\ (src/lib/ffi-callback.f) arms and reads its C entry through. The definition
\ after a CALLBACK-STUB! seals with NCBSTUB armed (src/habu/t0-emit.f THE
\ CALLBACK STUB); that compile ends the pending stub on every exit, as tier 1's
\ does. T0 classifies no body for the host: each reason is UNKNOWN, and a
\ quotation's arity is unknown (-1 -1), as tier 1 answers for one it has none
\ for. T0 compiles no shadow routine, even while a shadow is open
\ (test/x86-64-link-records.f stray).
\
\ The scan's recording unit latches the facts the capture and the call targets
\ read. T0's observer keeps the verdict the scan reached, as tier 1's tape does
\ (src/compiler/native/feed.f ON-DONE), and lays the tape the walk reads (THE
\ TAPE): a row per token the scan reports, as it reports it, and at the scan's
\ end each row's event, which the checker answers only then.
\ A does> definer is scanned twice: its head (CHECK-DOES-HEAD) from the
\ definition's start, then its clause (CHECK-DOES-CLAUSE) from M-DOES. The
\ clause's ordinals go on from the head's, one past for the `does>` between
\ them, which the checker files nothing for (CHECKER-OWNER:TAPE-ADVANCE), and
\ its token offsets count from the clause's start; the clause's scan first lays
\ the `does>` row, so the tape is in text order with the checker's ordinals.
variable OBSERVER-ID
variable SCAN-VERDICT
PTR-VARIABLE SCAN-TXT                \ the text the scan reads
variable SCAN-BASE                   \ its offset in the definition's text
variable SCAN-LO                     \ the ordinal of its first row

: LAY-ROW ( n n -- n ) {: off:n u:n :}
   ROWS @ {: k:n :}
   k 1+ {: m:n :}
   m ROW-OFF-RESERVE  m ROW-LEN-RESERVE  m ROW-KIND-RESERVE  m ROW-A0-RESERVE
   m ROW-A1-RESERVE  m ROW-U-RESERVE  m ROW-AT-RESERVE
   off k ROW-OFF !  u k ROW-LEN !  K-NONE k ROW-KIND !  0 k ROW-A0 !  0 k ROW-A1 !
   0 k ROW-U !  -1 k ROW-AT !
   m ROWS !
   k ;

: KEEP-BODY ( ptr u8 n n -- ) {: a:ptr u:n k:n :}
   BODIES-N @ {: at:n :}
   at u + BODIES-RESERVE
   a at BODIES u BYTE-COPY
   at k ROW-AT !
   at u + BODIES-N ! ;

: ON-SCAN ( ptr u8 n -- )
   drop {: a:ptr :}
   a SCAN-TXT !
   a SRC$ drop - {: base:n :}
   base SCAN-BASE !
   base 0= if
      0 ROWS !  0 BODIES-N !  -1 SPLIT !
      0 MEASURES !  0 BIND-COUNT !  0 HEAD-BINDS !
   else
      base DOES-SPAN -  DOES-SPAN 1-  LAY-ROW SPLIT !
      ROWS @ CLAUSE-ORD !
   then
   ROWS @ SCAN-LO ! ;

\ An escaped string's body is copied: it is not the text at its offset. Row 0,
\ the definition's name, names the unit from here on, as tier 1's
\ KEEP-TAPE-NAME.
: ON-TOKEN ( ptr u8 n n n n n -- )
   drop {: a:ptr u:n off:n ru:n kind:n :}
   SCAN-TXT @ off + {: at:ptr :}
   SCAN-BASE @ off + ru LAY-ROW {: k:n :}
   kind STRING-KIND? if
      u k ROW-U !
      a at <> if a u k KEEP-BODY then
   then
   k 0= if at NAME-A !  ru NAME-U ! then ;

: KEEP-MEASURES ( -- )
   SCAN-BASE @ {: base:n :}
   CHECKER-OWNER:WIDTH-ROWS {: rows:n :}
   MEASURES @ {: m:n :}
   m rows + {: total:n :}
   total MEASURE-AT-RESERVE  total MEASURE-POS-RESERVE  total MEASURE-CELLS-RESERVE
   rows 0 ?do
      i CHECKER-OWNER:WIDTH-ROW {: off:n pos:n w:n :}
      m i + {: k:n :}
      off base + k MEASURE-AT !  pos k MEASURE-POS !  w k MEASURE-CELLS !
   loop
   total MEASURES !
   CHECKER-OWNER:LOCAL-COUNT {: binds:n :}
   BIND-COUNT @ {: b:n :}
   b binds + BIND-WIDTHS-RESERVE
   binds 0 ?do i CHECKER-OWNER:LOCAL-WIDTH  b i + BIND-WIDTHS ! loop
   b binds + BIND-COUNT !
   base 0= if BIND-COUNT @ HEAD-BINDS ! then ;

\ Offsets are made whole by the scan's base.
: ON-DONE ( ptr u8 n n -- )
   SCAN-VERDICT !  2drop
   ROWS @ SCAN-LO @ ?do
      i CHECKER-OWNER:TAPE-EVENT  i ROW-A1 !  i ROW-A0 !  i ROW-KIND !
   loop
   KEEP-MEASURES ;

\ The buffers keep their mappings, grown to the largest definition seen: the
\ counts bound every read. Image capture unmaps them through the dynamic
\ storage registry.
: FORGET-MEASURES ( -- )
   0 MEASURES !  0 BIND-COUNT !  0 HEAD-BINDS ! ;

: OBSERVE ( -- )
   OBSERVER-ID BYTE-VIEW NULL-PTR BYTE-VIEW -
   [: ON-SCAN ;] [: ON-TOKEN ;] [: ON-DONE ;]
   CHECKER-OWNER:TAPE-INSTALL ;

\ A does> clause is named for a refusal by the definition's name.
: CHECK-UNIT ( -- n )
   M-DOES @ 0<> if
      CHECK-DOES-HEAD
      NAME$ CHECK-DOES-CLAUSE exit
   then
   CHECK-SOURCE ;

\ The scan's verdict stands over a hook's nonzero answer; a hook's zero answer
\ stands over the scan's, as tier 1's END-RECORDED has it.
: SCAN ( -- )
   OBSERVE
   0 SCAN-VERDICT !
   CHECKER-OWNER:TAPE-ARM
   [: CHECK-UNIT M-VERDICT ! ;] [: CHECKER-OWNER:TAPE-DISARM ;] finally
   M-VERDICT @ 0<> if SCAN-VERDICT @ M-VERDICT ! then
   DECLARE-ROW ;

: LOWER ( n -- ) {: before:n :}
   before SOURCE-PUBLICATION-CK
   RECORD-NAME-CK
   KEEP-ARITY
   WALK  CLAUSE @ M-DOES-FUN !
   NQ @ [: drop -1 -1 ;] HOST-ARITIES
   NHOST:SOURCE-FREEZE
   STUB-ARM
   NPUB:NEXT-SLOT T0-EMIT:SEAL
   STUB-TAKE
   PUBLISH-IT ;

\ The compile window, the emission and the armed stub end on every exit.
: SHUT ( -- )
   0 CHECKER-OWNER:WRITE-WINDOW
   NEMIT:CLEAR
   NCBSTUB:CLEAR ;

: RUN ( -- )
   ndict@ {: before:n :}
   SCAN
   VERDICT-CK
   E-NCOMP-STATE CHECKER-OWNER:WRITE-WINDOW
   before [: LOWER ;] [: SHUT ;] finally ;

: REFUSED$ ( n -- ptr u8 n ) {: rc:n :}
   rc E-T0-UNSUPPORTED =  rc E-UNMODELED-IMMEDIATE = or  rc E-T0-CONTROL = or
   rc E-T0-MATCH = or  rc E-T0-DEFER = or
   if UNSUPPORTED$ exit then
   NULL-PTR 0 ;

public

\ Compile the pending definition whose body text the engine captured.
: COMPILE ( ptr u8 n -- )
   IDLE-CK
   false STAGE-UNIT
   NHOST:UNKNOWN 0 NHOST:SOURCE-REFUSE
   CHECKER-OWNER:RESET-REPORT
   0 MISSING !  0 BAD-LEN !
   1 M-OPEN !
   [: RUN ;] catch {: rc:n :}
   0 M-OPEN !
   STUB-END
   NFETCH:RELEASE
   FORGET-MEASURES
   NHOST:SOURCE-ABANDON
   rc REFUSED$ {: a:ptr u:n :}
   0 rc  rc E-T0-UNSUPPORTED = MISSING @ 0<> and  a u a SETTLE-UNIT ;

: DISPATCH! ( [ ptr u8 n -- ] -- )
   data-base NCOMP-DISPATCH:COMP-CELL + xt! ;

: INSTALL ( -- )
   ['] CALLBACK-STUB! data-base NCOMP-DISPATCH:CALLBACK-STUB-CELL + xt!
   ['] CALLBACK-STUB@ data-base NCOMP-DISPATCH:CALLBACK-ENTRY-CELL + xt!
   ['] COMPILE DISPATCH! ;

;using
;package

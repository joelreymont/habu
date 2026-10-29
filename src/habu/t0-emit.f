\ t0-emit.f - the emitter seam of the tier 0 compiler, package T0-EMIT.
\
\ src/habu/t0.f (T0) drives the operations below; an emitter file per target
\ implements them (src/habu/t0-x64.f, src/habu/t0-arm64.f). The front end
\ names values by vreg (0..VREGS-1), never by machine register, so the types,
\ constants and rules here hold on every target.

require src/lib/type/deftype.f
require src/compiler/target.f
require src/compiler/native/emission.f
require src/compiler/native/callback-stub.f

package T0-EMIT
public

\ Tier 0 emitter seam: -9590..-9599, the block src/lib/errors.f names.
-9590 constant E-FIRST
-9599 constant E-LAST
-9590 constant E-T0-TARGET   \ SELECT of an architecture no emitter provided
-9591 constant E-T0-UNIT     \ QUOT-OPEN of a unit other than the next one
-9592 constant E-T0-UNFILLED \ an operation the selected emitter does not provide
-9593 constant E-T0-SLOT     \ code after the does> patch slot

DEFTYPE VREG       \ a virtual register, 0 .. VREGS-1
DEFTYPE ALU        \ a two-operand operation OP applies
DEFTYPE SHIFTER    \ a shift SHIFT applies
DEFTYPE CC         \ a condition SET-FLAG reads from the last compare
DEFTYPE WIDTH      \ the width LOAD moves

: OP-ADD ( -- alu ) 0 >ALU ;
: OP-SUB ( -- alu ) 1 >ALU ;
: OP-AND ( -- alu ) 2 >ALU ;
: OP-OR  ( -- alu ) 3 >ALU ;
: OP-XOR ( -- alu ) 4 >ALU ;

: SH-LEFT  ( -- shifter ) 0 >SHIFTER ;
: SH-RIGHT ( -- shifter ) 1 >SHIFTER ;

\ The comparisons, signed; each emitter maps them to its own conditions.
: CC-EQ  ( -- cc ) 0 >CC ;
: CC-NE  ( -- cc ) 1 >CC ;
: CC-LT  ( -- cc ) 2 >CC ;
: CC-GE  ( -- cc ) 3 >CC ;
: CC-LE  ( -- cc ) 4 >CC ;
: CC-GT  ( -- cc ) 5 >CC ;

: CELL-W ( -- width ) 8 >WIDTH ;
: BYTE-W ( -- width ) 1 >WIDTH ;

\ The comparison that answers the same with its operands exchanged.
: CC-SWAP ( cc -- cc ) {: c:cc :}
   c CC>N {: k:n :}
   k 2 = if CC-GT exit then
   k 5 = if CC-LT exit then
   k 3 = if CC-LE exit then
   k 4 = if CC-GE exit then
   c ;

\ The comparison that holds exactly when c fails: the numbering pairs each
\ comparison with its negation, EQ NE, LT GE, LE GT.
: CC-NOT ( cc -- cc ) CC>N 1 xor >CC ;

\ The address row kind of a quotation, past every NEMIT address kind: sealing
\ files it as NEMIT:ADDR-CODE.
NEMIT:ADDR-KIND-MAX 1+ constant ADDR-QUOT-KIND

\ ---- the architectures -------------------------------------------------------
7 constant ARCHES

private

ARCHES TYPED-BUFFER INSTALLED bool

: SLOT ( CTARGET:arch -- n )
   MATCH CTARGET:arch
      aarch64 OF 0 ENDOF
      ptx     OF 1 ENDOF
      a32     OF 2 ENDOF
      thumb2  OF 3 ENDOF
      c66x    OF 4 ENDOF
      x86-64  OF 5 ENDOF
      wasm    OF 6 ENDOF
   ;MATCH ;

\ The selected architecture's slot; -1, no slot, until SELECT.
variable CUR
-1 CUR !
TYPED-VARIABLE CUR-ARCH CTARGET:arch  \ and the architecture itself

public

\ The selected slot, for a layer that keeps per-architecture cells of its own
\ (src/habu/t0-label.f).
: SELECTED ( -- n ) CUR @ ;

private

\ ---- the rows ----------------------------------------------------------------
\ THE ROWS. CALL-ROW+ files a call or tail site ( at kind target loc ), kind
\ NEMIT:CALL or NEMIT:TAIL, loc the calling token's byte offset in the body
\ text. ADDR-ROW+ files an address site ( at kind target origin ), kind
\ NEMIT:ADDR-DATA, NEMIT:ADDR-CODE or ADDR-QUOT-KIND; a data address's origin
\ comes with it through ADDR-DATA (a string literal's is the one
\ NSTR:INTERN-SOURCE answers), and a code or quotation address's is -1. at is
\ the instruction's first byte from the emission's byte 0. A quotation's
\ target is its unit number; on x86-64 its instruction holds the unit's offset,
\ which QUOT-OPEN writes. An emitter whose quotation address is relative to its
\ own site (arm64's `adr`) files no row for it.
variable N-UNITS
DYNAMIC-BUFFER UNIT-AT n              \ where each unit starts
variable N-CALLS
DYNAMIC-BUFFER CALL-AT n              \ call row: first byte, kind, target, loc
DYNAMIC-BUFFER CALL-KIND-AT n
DYNAMIC-BUFFER CALL-TO n
DYNAMIC-BUFFER CALL-LOC n
variable N-ADDRS
DYNAMIC-BUFFER ADDR-AT n              \ address row: first byte, kind, target,
DYNAMIC-BUFFER ADDR-KIND-AT n         \ origin
DYNAMIC-BUFFER ADDR-TO n
DYNAMIC-BUFFER ADDR-ORIGIN n
variable SLOT-AT                      \ the patch slot's offset, or -1

: ROWS-CLEAR ( -- ) 0 N-UNITS !  0 N-CALLS !  0 N-ADDRS !  -1 SLOT-AT ! ;
: SLOT? ( -- bool ) SLOT-AT @ 0 >= ;

public

\ File a unit's start, a call or tail site and an address site.
: UNIT+ ( n -- ) {: at:n :}
   N-UNITS @ {: k:n :}
   k 1+ UNIT-AT-RESERVE
   at k UNIT-AT !
   k 1+ N-UNITS ! ;

: CALL-ROW+ ( n n n n -- ) {: at:n kind:n to:n loc:n :}
   N-CALLS @ {: k:n :}
   k 1+ CALL-AT-RESERVE  k 1+ CALL-KIND-AT-RESERVE
   k 1+ CALL-TO-RESERVE  k 1+ CALL-LOC-RESERVE
   at k CALL-AT !  kind k CALL-KIND-AT !  to k CALL-TO !  loc k CALL-LOC !
   k 1+ N-CALLS ! ;

: ADDR-ROW+ ( n n n n -- ) {: at:n kind:n to:n origin:n :}
   N-ADDRS @ {: k:n :}
   k 1+ ADDR-AT-RESERVE  k 1+ ADDR-KIND-AT-RESERVE  k 1+ ADDR-TO-RESERVE
   k 1+ ADDR-ORIGIN-RESERVE
   at k ADDR-AT !  kind k ADDR-KIND-AT !  to k ADDR-TO !  origin k ADDR-ORIGIN !
   k 1+ N-ADDRS ! ;

\ File the offset of the return SLOT-CLOSE left by.
: PATCH-SLOT! ( n -- ) SLOT-AT ! ;

\ The rows read back, for an emitter's PATCH: unit k's start, and call row and
\ address row k as ( at kind target ).
: UNIT ( n -- n ) UNIT-AT @ ;
: CALL-ROWS ( -- n ) N-CALLS @ ;
: CALL-ROW ( n -- n n n ) {: k:n :}  k CALL-AT @  k CALL-KIND-AT @  k CALL-TO @ ;
: ADDR-ROWS ( -- n ) N-ADDRS @ ;
: ADDR-ROW ( n -- n n n ) {: k:n :}  k ADDR-AT @  k ADDR-KIND-AT @  k ADDR-TO @ ;

ARCHES TYPED-BUFFER VREGS-N n
: VREGS ( -- n ) CUR @ VREGS-N @ ;

\ ---- the body ----------------------------------------------------------------
\ THE SINK. OPEN empties the emitter's sink and the seam's rows; CLOSE ends a
\ unit; SEAL hands the emission to NEMIT (src/compiler/native/emission.f). The
\ caller runs NEMIT:CLEAR before the next OPEN, on the accepting and the
\ refusing path alike. NEMIT reads the bytes in place, so they stay the
\ emitter's until the next OPEN.
\
\ THE UNITS. A definition and its quotations are one emission: unit 0 is the
\ definition at byte 0, and each quotation is a later unit that QUOT-OPEN
\ begins where the previous CLOSE left off. A `does>` clause is a later unit
\ too, begun by QUOT-OPEN when the parent pushed its address, else by
\ UNIT-OPEN. Sealing files one NEMIT function per unit. `recurse` in any unit
\ calls byte 0, as both tiers run it, and files no row.
\
\ THE PATCH SLOT. SLOT-CLOSE closes a unit whose return is the does> patch
\ slot (PATCH-SLOT!, sealed as NEMIT:PATCH-SLOT!). The kernel finds the slot
\ from the record's length, so that return is the emission's last instruction:
\ a unit begun after it, or a callback stub with it, throws E-T0-SLOT.
ARCHES TYPED-BUFFER OPEN-XT [ -- ]
ARCHES TYPED-BUFFER QUOT-OPEN-XT [ n -- ]
ARCHES TYPED-BUFFER UNIT-OPEN-XT [ -- ]
ARCHES TYPED-BUFFER RETURN-XT [ -- ]
ARCHES TYPED-BUFFER CLOSE-XT [ -- ]
ARCHES TYPED-BUFFER BYTES-XT [ -- ptr u8 n ]
ARCHES TYPED-BUFFER PATCH-XT [ n -- ]
ARCHES TYPED-BUFFER SLOT-CLOSE-XT [ -- ]
ARCHES TYPED-BUFFER CB-STUB-XT [ -- n ]

private

\ An xt as the number its cell holds: 0 while unset.
CAST: XT>N ( [ R -- S ] -- n )

\ The xt of an operation not every emitter provides; unset, E-T0-UNFILLED.
: FILLED ( [ R -- S ] -- [ R -- S ] )
   dup XT>N 0= if E-T0-UNFILLED throw then ;

\ The finished emission, once the last unit is closed.
: BYTES ( -- ptr u8 n ) CUR @ BYTES-XT @ execute ;

\ Write every call row's displacement and every quotation address row for the
\ emission placed at address n.
: PATCH ( n -- ) CUR @ PATCH-XT @ execute ;

public

: OPEN ( -- ) ROWS-CLEAR  CUR @ OPEN-XT @ execute ;

\ Begin quotation unit n, which ADDR-QUOT named, and bind the address
\ ADDR-QUOT emitted to it. Units begin in order, so n is the count begun so
\ far.
: QUOT-OPEN ( n -- )
   SLOT? if E-T0-SLOT throw then
   CUR @ QUOT-OPEN-XT @ execute ;

\ Begin the next unit with no address bound to it, for a does> definer's
\ empty clause, whose parent pushes no clause address.
: UNIT-OPEN ( -- )
   SLOT? if E-T0-SLOT throw then
   CUR @ UNIT-OPEN-XT @ FILLED execute ;

\ Leave the word, from the middle of its body or at its end.
: RETURN ( -- ) CUR @ RETURN-XT @ execute ;

: CLOSE ( -- ) CUR @ CLOSE-XT @ execute ;

\ Leave the word by a return that is the does> patch slot, and close the unit
\ (THE PATCH SLOT).
: SLOT-CLOSE ( -- ) CUR @ SLOT-CLOSE-XT @ FILLED execute ;

\ ---- values ------------------------------------------------------------------
ARCHES TYPED-BUFFER LIT-XT [ n vreg -- ]
ARCHES TYPED-BUFFER ADDR-DATA-XT [ n n vreg -- ]
ARCHES TYPED-BUFFER ADDR-CODE-XT [ n vreg -- ]
ARCHES TYPED-BUFFER ADDR-QUOT-XT [ n vreg -- ]
ARCHES TYPED-BUFFER PUSH-XT [ vreg n -- ]
ARCHES TYPED-BUFFER PUSH-LIT-XT [ n n -- ]
ARCHES TYPED-BUFFER POP-XT [ vreg n -- ]
ARCHES TYPED-BUFFER SPILL-XT [ vreg n -- ]
ARCHES TYPED-BUFFER SPILL-LIT-XT [ n n -- ]
ARCHES TYPED-BUFFER FILL-XT [ vreg n -- ]
ARCHES TYPED-BUFFER DSP+-XT [ n -- ]
ARCHES TYPED-BUFFER MOV-XT [ vreg vreg -- ]
ARCHES TYPED-BUFFER EXCHANGE-XT [ vreg vreg -- ]
ARCHES TYPED-BUFFER SLOTS-XT [ n -- ]
ARCHES TYPED-BUFFER SLOT-STORE-XT [ vreg n -- ]
ARCHES TYPED-BUFFER SLOT-LOAD-XT [ vreg n -- ]

: LIT ( n vreg -- ) CUR @ LIT-XT @ execute ;
\ Data address v, whose address row files origin o (THE ROWS).
: ADDR-DATA ( n n vreg -- ) CUR @ ADDR-DATA-XT @ execute ;
: ADDR-CODE ( n vreg -- ) CUR @ ADDR-CODE-XT @ execute ;

\ The address of quotation unit n, which QUOT-OPEN begins later.
: ADDR-QUOT ( n vreg -- ) CUR @ ADDR-QUOT-XT @ execute ;

\ The data stack grows up. SPILL, SPILL-LIT and FILL address the cell k cells
\ above the stack pointer without moving it; DSP+ moves it by k cells. PUSH and
\ PUSH-LIT store at the pointer then move it up k cells, and POP moves it down
\ k cells then loads: with k 1 a push and a pop, otherwise the last transfer of
\ a k-cell spill or fill, so the move costs no instruction of its own where the
\ target writes back.
: PUSH ( vreg n -- ) CUR @ PUSH-XT @ execute ;
: PUSH-LIT ( n n -- ) CUR @ PUSH-LIT-XT @ execute ;
: POP ( vreg n -- ) CUR @ POP-XT @ execute ;
: SPILL ( vreg n -- ) CUR @ SPILL-XT @ execute ;
: SPILL-LIT ( n n -- ) CUR @ SPILL-LIT-XT @ execute ;
: FILL ( vreg n -- ) CUR @ FILL-XT @ execute ;
: DSP+ ( n -- ) CUR @ DSP+-XT @ execute ;
: MOV ( vreg vreg -- ) CUR @ MOV-XT @ execute ;
\ The two vregs' values swapped, a join's register cycle.
: EXCHANGE ( vreg vreg -- ) CUR @ EXCHANGE-XT @ execute ;

\ The frame: SLOTS makes it hold at least n cells, SLOT-STORE and SLOT-LOAD
\ move a cell between a vreg and slot k.
: SLOTS ( n -- ) CUR @ SLOTS-XT @ execute ;
: SLOT-STORE ( vreg n -- ) CUR @ SLOT-STORE-XT @ execute ;
: SLOT-LOAD ( vreg n -- ) CUR @ SLOT-LOAD-XT @ execute ;

\ ---- arithmetic --------------------------------------------------------------
ARCHES TYPED-BUFFER OP-XT [ vreg vreg alu -- ]
ARCHES TYPED-BUFFER OP-LIT-XT [ vreg n alu -- ]
ARCHES TYPED-BUFFER SHIFT-XT [ vreg vreg shifter -- ]
ARCHES TYPED-BUFFER SHIFT-LIT-XT [ vreg n shifter -- ]
ARCHES TYPED-BUFFER MULX-XT [ vreg vreg -- ]
ARCHES TYPED-BUFFER MUL-LIT-XT [ vreg n -- ]
ARCHES TYPED-BUFFER DIV-LIT-XT [ vreg n -- ]
ARCHES TYPED-BUFFER MOD-LIT-XT [ vreg n -- ]
ARCHES TYPED-BUFFER NEG-XT [ vreg -- ]
ARCHES TYPED-BUFFER NOT-XT [ vreg -- ]
ARCHES TYPED-BUFFER CMP-XT [ vreg vreg -- ]
ARCHES TYPED-BUFFER CMP-LIT-XT [ vreg n -- ]
ARCHES TYPED-BUFFER SET-FLAG-XT [ vreg cc -- ]
ARCHES TYPED-BUFFER IMM?-XT [ n -- bool ]

\ Whether OP-LIT, MUL-LIT, CMP-LIT and OF-LIT take n as their immediate;
\ each of them requires n to answer true.
: IMM? ( n -- bool ) CUR @ IMM?-XT @ execute ;

: OP ( vreg vreg alu -- ) CUR @ OP-XT @ execute ;
: OP-LIT ( vreg n alu -- ) CUR @ OP-LIT-XT @ execute ;

\ A shift count is masked to six bits, as Habu's `lshift` and `rshift` are (a
\ count of 64 shifts by nothing); SH-RIGHT is logical.
: SHIFT ( vreg vreg shifter -- ) CUR @ SHIFT-XT @ execute ;
: SHIFT-LIT ( vreg n shifter -- ) CUR @ SHIFT-LIT-XT @ execute ;

: MULX ( vreg vreg -- ) CUR @ MULX-XT @ execute ;
: MUL-LIT ( vreg n -- ) CUR @ MUL-LIT-XT @ execute ;

\ DIVISION. `/` and `mod` by a literal divisor other than 0 and -1 are DIV-LIT
\ and MOD-LIT. A zero divisor stays a call of the kernel row, which throws
\ E-DIV-ZERO; -1 stays a call because x86-64 idiv faults on MIN-N -1 (arm64
\ sdiv answers MIN-N), and one rule holds for every target. A register divisor
\ and `/mod` stay calls too.
\
\ DIV-LIT and MOD-LIT replace the dividend in d with its quotient by n,
\ truncated toward zero, or the remainder that carries the dividend's sign, as
\ the kernel's `/` and `mod` rows compute them; n is neither 0 nor -1. Every
\ other vreg keeps its value.
: DIV-LIT ( vreg n -- ) CUR @ DIV-LIT-XT @ execute ;
: MOD-LIT ( vreg n -- ) CUR @ MOD-LIT-XT @ execute ;
: NEG ( vreg -- ) CUR @ NEG-XT @ execute ;
: NOT ( vreg -- ) CUR @ NOT-XT @ execute ;

\ CMP and CMP-LIT compare a with b (or with n) and set the condition codes,
\ writing no vreg; SET-FLAG makes d -1 when condition c of the last compare
\ holds, else 0: a Habu flag. IF-CC,, WHILE-CC, and UNTIL-CC, branch on the
\ codes instead. Between a compare and the word that reads its codes the
\ front end emits only moves: PUSH, PUSH-LIT, SPILL, SPILL-LIT, FILL, POP, LIT,
\ ADDR-DATA, ADDR-CODE, ADDR-QUOT, DSP+, MOV and EXCHANGE. Every emitter keeps
\ those condition-preserving (x86-64's DSP+ is an lea, its zero literal a mov,
\ its exchange an xchg, its pop an lea and a mov; arm64's exchange is three
\ movs, its stack and address offsets add and sub without flags).
: CMP ( vreg vreg -- ) CUR @ CMP-XT @ execute ;
: CMP-LIT ( vreg n -- ) CUR @ CMP-LIT-XT @ execute ;
: SET-FLAG ( vreg cc -- ) CUR @ SET-FLAG-XT @ execute ;

\ ---- memory ------------------------------------------------------------------
ARCHES TYPED-BUFFER LOAD-XT [ vreg vreg width -- ]

\ LOAD reads the cell or the zero-extended byte at address a into d, and does
\ not screen the address, as the kernel's `@` and `c@` rows do not. `!` and
\ `c!` screen a span and stay calls, so the seam has no store.
: LOAD ( vreg vreg width -- ) CUR @ LOAD-XT @ execute ;

\ ---- calls -------------------------------------------------------------------
ARCHES TYPED-BUFFER CALL-XT [ n n -- ]
ARCHES TYPED-BUFFER TAIL-XT [ n n -- ]
ARCHES TYPED-BUFFER CALL-SELF-XT [ -- ]

\ A call or tail branch to address to, for the token at body offset loc.
: CALL ( n n -- ) CUR @ CALL-XT @ execute ;
: TAIL ( n n -- ) CUR @ TAIL-XT @ execute ;

\ `recurse`: a call of byte 0, the definition, from any unit.
: CALL-SELF ( -- ) CUR @ CALL-SELF-XT @ execute ;

\ ---- control -----------------------------------------------------------------
\ CONTROL. The front end refuses a structure out of order before calling one,
\ so these words trust the nesting. Branches, targets and open loop levels stay
\ behind the seam: an emitter that branches by label installs
\ src/habu/t0-label.f. The words a source keyword names end in a comma, since
\ a keyword cannot be a definition name.
ARCHES TYPED-BUFFER IF,-XT [ vreg -- ]
ARCHES TYPED-BUFFER ELSE,-XT [ -- ]
ARCHES TYPED-BUFFER THEN,-XT [ -- ]
ARCHES TYPED-BUFFER BEGIN,-XT [ -- ]
ARCHES TYPED-BUFFER WHILE,-XT [ vreg -- ]
ARCHES TYPED-BUFFER REPEAT,-XT [ -- ]
ARCHES TYPED-BUFFER UNTIL,-XT [ vreg -- ]
ARCHES TYPED-BUFFER IF-CC,-XT [ cc -- ]
ARCHES TYPED-BUFFER WHILE-CC,-XT [ cc -- ]
ARCHES TYPED-BUFFER UNTIL-CC,-XT [ cc -- ]
ARCHES TYPED-BUFFER AGAIN,-XT [ -- ]
ARCHES TYPED-BUFFER CASE,-XT [ -- ]
ARCHES TYPED-BUFFER OF,-XT [ vreg vreg -- ]
ARCHES TYPED-BUFFER OF-LIT-XT [ vreg n -- ]
ARCHES TYPED-BUFFER ENDOF,-XT [ -- ]
ARCHES TYPED-BUFFER ENDCASE,-XT [ -- ]

\ `if`: past the arm when the vreg is zero.
: IF, ( vreg -- ) CUR @ IF,-XT @ execute ;

\ `if` on condition c of the last compare: past the arm when c fails.
: IF-CC, ( cc -- ) CUR @ IF-CC,-XT @ execute ;

\ `else`: past the second arm, then the open `if` lands here.
: ELSE, ( -- ) CUR @ ELSE,-XT @ execute ;

\ `then`: the innermost open forward branch lands here.
: THEN, ( -- ) CUR @ THEN,-XT @ execute ;

: BEGIN, ( -- ) CUR @ BEGIN,-XT @ execute ;

\ `while`: out of the loop when the vreg is zero; the exit stays open beneath
\ the head, as Forth orders orig and dest, so `begin ... while ... while ...
\ repeat then` resolves.
: WHILE, ( vreg -- ) CUR @ WHILE,-XT @ execute ;

\ `while` on condition c of the last compare: out of the loop when c fails.
: WHILE-CC, ( cc -- ) CUR @ WHILE-CC,-XT @ execute ;

\ `repeat`: back to the head, then the innermost `while` exit lands here.
: REPEAT, ( -- ) CUR @ REPEAT,-XT @ execute ;

\ `until`: back to the head when the vreg is zero.
: UNTIL, ( vreg -- ) CUR @ UNTIL,-XT @ execute ;

\ `until` on condition c of the last compare: back to the head when c fails.
: UNTIL-CC, ( cc -- ) CUR @ UNTIL-CC,-XT @ execute ;

: AGAIN, ( -- ) CUR @ AGAIN,-XT @ execute ;

\ `case` opens; OF, and OF-LIT branch past the arm when the selector differs
\ from the vreg or from n, which must answer IMM? true; ENDOF, branches to the
\ end and lands the open `of` here; ENDCASE, lands every `endof` exit here.
: CASE, ( -- ) CUR @ CASE,-XT @ execute ;
: OF, ( vreg vreg -- ) CUR @ OF,-XT @ execute ;
: OF-LIT ( vreg n -- ) CUR @ OF-LIT-XT @ execute ;
: ENDOF, ( -- ) CUR @ ENDOF,-XT @ execute ;
: ENDCASE, ( -- ) CUR @ ENDCASE,-XT @ execute ;

\ ---- the return stack --------------------------------------------------------
ARCHES TYPED-BUFFER TOR-XT [ ptr vreg n -- ]
ARCHES TYPED-BUFFER RFROM-XT [ ptr vreg n -- ]
ARCHES TYPED-BUFFER RFETCH-XT [ ptr vreg n n -- ]

\ The return stack grows up from STACK-ABI:RETURN-BASE-CELL and is RSP-CELL
\ cells deep (src/habu/layout.f RSP-CELL). Each operation moves the cells of
\ the n vregs at p, at most VREGS, the first the deepest of n adjacent slots,
\ and reads the depth once, as the JIT's LP2RS (src/habu/habu2.f) moves a
\ block: TOR stores them in the slots above the top and RFROM loads the top n
\ slots, each writing the depth once; RFETCH loads the n slots whose
\ highest is k under the top and writes nothing.
: TOR ( ptr vreg n -- ) CUR @ TOR-XT @ execute ;
: RFROM ( ptr vreg n -- ) CUR @ RFROM-XT @ execute ;
: RFETCH ( ptr vreg n n -- ) CUR @ RFETCH-XT @ execute ;

: VREG-AT ( ptr vreg n -- vreg ) cells + @ ;

\ ---- loops -------------------------------------------------------------------
ARCHES TYPED-BUFFER LOOP-PUSH-XT [ vreg vreg -- ]
ARCHES TYPED-BUFFER LOOP-POP-XT [ -- ]
ARCHES TYPED-BUFFER LOOP-I-XT [ vreg -- ]
ARCHES TYPED-BUFFER LOOP-J-XT [ vreg -- ]
ARCHES TYPED-BUFFER LOOP-OPEN-XT [ vreg vreg bool -- ]
ARCHES TYPED-BUFFER LEAVE,-XT [ -- ]
ARCHES TYPED-BUFFER LOOP-NEXT-XT [ vreg -- ]
ARCHES TYPED-BUFFER LOOP-STEP-XT [ vreg vreg -- ]
ARCHES TYPED-BUFFER LOOP-CLOSE-XT [ -- ]

\ The DO/LOOP frames live on the DATA loop stack, where `j`, catch
\ (src/habu/kernel-x64.f CAUGHT,) and the arm64 tier 0 (src/habu/habu2.f
\ J-FRAME) find them: LOOPSP-CELL counts the frames and frame k, 16 bytes at
\ STACK-ABI:LOOP-BASE-CELL's base + 16k, holds the index, its home, then the
\ limit. `loop` steps while index < limit, signed; `+loop` steps until the
\ index crosses the boundary between limit - 1 and limit in the step's
\ direction; `?do` enters while start < limit, and LOOP-STEP makes the entry
\ of its level skip equal bounds only. LOOP-OPEN opens a level; LOOP-NEXT,
\ LOOP-STEP and LEAVE, branch to the innermost level's head or past its end;
\ LOOP-CLOSE lands its exits on the pop of its frame. The loop words write no
\ vreg but those passed in, so values in the others live across every head,
\ back edge and exit.

\ Push a frame of index s and limit l; pop the innermost frame.
: LOOP-PUSH ( vreg vreg -- ) CUR @ LOOP-PUSH-XT @ execute ;
: LOOP-POP ( -- ) CUR @ LOOP-POP-XT @ execute ;

\ The index of the innermost frame and of the one outside it into the vreg.
: LOOP-I ( vreg -- ) CUR @ LOOP-I-XT @ execute ;
: LOOP-J ( vreg -- ) CUR @ LOOP-J-XT @ execute ;

\ Open a level whose head is here. With entry true it is `?do`'s: past the
\ level unless start s < limit l.
: LOOP-OPEN ( vreg vreg bool -- ) CUR @ LOOP-OPEN-XT @ execute ;

\ `leave`: past the innermost level.
: LEAVE, ( -- ) CUR @ LEAVE,-XT @ execute ;

\ `loop`: step the index by one and branch back to the head while it is below
\ the limit. t is a free vreg it may clobber.
: LOOP-NEXT ( vreg -- ) CUR @ LOOP-NEXT-XT @ execute ;

\ `+loop`: step the index by s and branch back to the head unless it crossed
\ the boundary. s and t are clobbered; t is a free vreg.
: LOOP-STEP ( vreg vreg -- ) CUR @ LOOP-STEP-XT @ execute ;

\ Land the innermost level's `leave` exits and entry here, close it and pop
\ its frame.
: LOOP-CLOSE ( -- ) CUR @ LOOP-CLOSE-XT @ execute ;

\ ---- the registry ------------------------------------------------------------
\ THE SEAM. Each operation executes the xt its cell holds for the selected
\ architecture; every cell, VREGS-N among them, has one slot per architecture.
\ An emitter calls PROVIDE with its architecture and an installer that stores
\ its words into that slot of every cell. SELECT picks the slot the operations
\ run, per definition if need be, and throws E-T0-TARGET for an architecture
\ no emitter provided. An unset cell stays unset in its slot, so an operation
\ never runs another target's code: UNIT-OPEN, SLOT-CLOSE and the callback stub
\ throw E-T0-UNFILLED, and any other unset cell ends the process with
\ `hb: unset quotation` (exit 86, ENGINE-ERROR:CALLABLE-ABI). An operation
\ before any SELECT throws E-LAYOUT-BOUNDS (7122).
: PROVIDE ( CTARGET:arch [ n -- ] -- )
   swap SLOT {: k:n :}
   k swap execute
   true k INSTALLED ! ;

: SELECT ( CTARGET:arch -- )
   {: a:CTARGET:arch :}
   a SLOT {: k:n :}
   k INSTALLED @ 0= if E-T0-TARGET throw then
   k CUR !
   a CUR-ARCH ! ;

\ ---- sealing -----------------------------------------------------------------
\ SEALING. SEAL n places the emission at address n: PATCH writes every call
\ row's displacement and every quotation address as n plus the unit's offset,
\ because publication (src/compiler/native/publish.f) only copies the bytes and
\ records relocations. SEAL opens NEMIT with no trailing return, so the record
\ is the emission's whole span, and files a quotation's row as NEMIT:ADDR-CODE.
\
\ THE CALLBACK STUB. While NCBSTUB (src/compiler/native/callback-stub.f) is
\ armed, sealing has the emitter append tier 1's stub after the last unit,
\ files it as the last function and records its placed address as
\ NCBSTUB:ENTRY!.
private

: NEMIT-KIND ( n -- n ) {: kind:n :}
   kind ADDR-QUOT-KIND = if NEMIT:ADDR-CODE exit then
   kind ;

\ The callback stub's offset once the emitter appended it, or -1 while
\ NCBSTUB is not armed (THE CALLBACK STUB).
: CB-STUB ( -- n )
   NCBSTUB:ARMED? 0= if -1 exit then
   SLOT? if E-T0-SLOT throw then
   CUR @ CB-STUB-XT @ FILLED execute ;

\ The stub goes in before BYTES answers the emission it ends.
: NEMIT-OPEN ( -- n )
   CB-STUB
   BYTES 0 CUR-ARCH @ NEMIT:OPEN ;

: NEMIT-ROWS ( n -- ) {: stub:n :}
   N-UNITS @ 0 ?do i UNIT-AT @ NEMIT:FUNCTION+ loop
   stub 0 >= if stub NEMIT:FUNCTION+ then
   SLOT? if SLOT-AT @ NEMIT:PATCH-SLOT! then
   N-CALLS @ 0 ?do
      i CALL-AT @  i CALL-KIND-AT @  i CALL-TO @  0  i CALL-LOC @  NEMIT:CALL-SITE+
   loop
   N-ADDRS @ 0 ?do
      i ADDR-AT @  i ADDR-KIND-AT @ NEMIT-KIND  i ADDR-ORIGIN @  NEMIT:ADDR-SITE+
   loop
   NEMIT:SEAL ;

public

: SEAL ( n -- ) {: at:n :}
   NEMIT-OPEN {: stub:n :}
   at PATCH
   at NEMIT:PLACE
   stub 0 >= if at stub + NCBSTUB:ENTRY! then
   stub NEMIT-ROWS ;

;package

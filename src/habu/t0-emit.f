\ t0-emit.f - the emitter seam of the tier 0 compiler, package T0-EMIT.
\
\ src/habu/t0.f (package T0) walks a definition's body text and drives the
\ operations below; an emitter file per target implements them
\ (src/habu/t0-x64.f for x86-64, src/habu/t0-arm64.f for arm64). The front
\ end names values by virtual register (vreg 0..VREGS-1) and never by machine
\ register, so it stays the same for every target, and the types, constants
\ and rules here mean the same on every target. An emitter file states only
\ its encodings and register choices.
\
\ THE SEAM. Each operation is a public word that executes the xt its cell
\ holds for the selected architecture. Every cell, VREGS-N among them, is a
\ buffer with one slot per architecture. An emitter file loads its words
\ without binding any of them here and calls PROVIDE with its architecture
\ and an installer, which PROVIDE runs once with that architecture's slot:
\ the installer stores the emitter's words and vreg count into that slot of
\ every cell. SELECT makes an architecture's slot the one the operations run,
\ so several emitters can be loaded in one process and one is selected at a
\ time, per definition if need be. SELECT of an architecture no emitter
\ provided throws E-T0-TARGET. A cell an emitter leaves unset stays unset in
\ its own slot, whatever another emitter installed, and executing it ends
\ the process with `hb: unset quotation` (exit 86, ENGINE-ERROR:CALLABLE-ABI,
\ src/habu/kernel-x64.f and src/habu/habu1.f), so an operation never runs
\ another target's code or garbage. An operation called before any SELECT
\ throws E-LAYOUT-BOUNDS (7122): no slot is selected.
\
\ THE SINK. OPEN begins an emission, emptying the emitter's sink and the
\ seam's rows; CLOSE ends a unit. Once the last unit is closed, SEAL or
\ SEAL-UNPLACED hands the emission to NEMIT (src/compiler/native/emission.f),
\ which then answers its bytes and rows until the caller runs NEMIT:CLEAR; the
\ caller runs it before the next OPEN, on the accepting and the refusing path
\ alike. NEMIT reads the bytes in place, so they stay the emitter's until the
\ next OPEN.
\
\ THE UNITS. A definition and its quotations are one emission: unit 0 is the
\ definition and starts at byte 0, and each quotation is a later unit, a
\ function of its own that QUOT-OPEN begins where the previous unit's CLOSE
\ left off. The emitter files each unit's start with UNIT+, and sealing files
\ one NEMIT function per unit. `recurse` in any unit calls byte 0, the
\ definition, as both tiers run it, and files no row.
\
\ THE ROWS are the seam's, filed by the emitter: CALL-ROW+ a call or tail site
\ ( at kind target loc ), kind NEMIT:CALL or NEMIT:TAIL, loc the byte offset of
\ the calling token in the body text; ADDR-ROW+ a code, data or quotation
\ address site ( at kind target ), kind NEMIT:ADDR-DATA, NEMIT:ADDR-CODE or
\ ADDR-QUOT-KIND. at is the instruction's first byte, from the emission's byte
\ 0, and each unit's rows follow the previous unit's. A quotation's target is
\ its unit number; on x86-64 the address its instruction holds is the unit's
\ offset, which QUOT-OPEN writes. An emitter whose quotation address is
\ relative to its own site (arm64's `adr`) files no row for it: QUOT-OPEN
\ writes the distance and no placement changes it. The emitter's PATCH reads
\ the rows back.
\
\ SEALING. SEAL n is the emission placed at address n: PATCH writes every call
\ row's displacement to its target and every quotation address row as n plus
\ the unit's offset, as publication (src/compiler/native/publish.f) copies the
\ bytes and only records relocations. SEAL-UNPLACED leaves the bytes as
\ emitted, every call field zero and every quotation address row its unit's
\ offset, the unplaced form a shadow reads (src/arch/x86-64/passes.f
\ SHADOW-ADDRS). Both open NEMIT for the selected architecture with no
\ trailing return, file the units, the call rows with implementation 0 and the
\ address rows with origin -1, a quotation's as NEMIT:ADDR-CODE, and seal it.
\
\ CONTROL. The front end drives the structured words and the counted-loop
\ words in the order the source nests them; it refuses a structure out of
\ order before calling one, so these words trust the nesting. The branches,
\ their targets and each open loop level stay in the emitter and are resolved
\ there. The words a source keyword names end in a comma, since a keyword
\ cannot be a definition name.
\
\ DIVISION. The seam has no divide: `/`, `mod` and `/mod` are calls of the
\ kernel rows, which screen a zero divisor and MIN-N -1.

require src/lib/type/deftype.f
require src/compiler/target.f
require src/compiler/native/emission.f

package T0-EMIT
public

\ Tier 0 emitter seam: -9590..-9599, the block src/lib/errors.f names.
-9590 constant E-FIRST
-9599 constant E-LAST
-9590 constant E-T0-TARGET   \ SELECT of an architecture no emitter provided
-9591 constant E-T0-UNIT     \ QUOT-OPEN of a unit other than the next one

DEFTYPE VREG       \ a virtual register, 0 .. VREGS-1
DEFTYPE ALU        \ a two-operand operation OP applies
DEFTYPE SHIFTER    \ a shift SHIFT applies
DEFTYPE CC         \ a comparison CMP-SET answers
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

\ The address row kind of a quotation, past every NEMIT address kind: sealing
\ files it as NEMIT:ADDR-CODE.
NEMIT:ADDR-KIND-MAX 1+ constant ADDR-QUOT-KIND

\ ---- the architectures -------------------------------------------------------
private

7 constant ARCHES
ARCHES TYPED-BUFFER INSTALLED bool

\ The slot an architecture's emitter fills in every cell.
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

\ ---- the rows ----------------------------------------------------------------
variable N-UNITS
DYNAMIC-BUFFER UNIT-AT n              \ where each unit starts
variable N-CALLS
DYNAMIC-BUFFER CALL-AT n              \ call row: first byte, kind, target, loc
DYNAMIC-BUFFER CALL-KIND-AT n
DYNAMIC-BUFFER CALL-TO n
DYNAMIC-BUFFER CALL-LOC n
variable N-ADDRS
DYNAMIC-BUFFER ADDR-AT n              \ address row: first byte, kind, target
DYNAMIC-BUFFER ADDR-KIND-AT n
DYNAMIC-BUFFER ADDR-TO n

: ROWS-CLEAR ( -- ) 0 N-UNITS !  0 N-CALLS !  0 N-ADDRS ! ;

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

: ADDR-ROW+ ( n n n -- ) {: at:n kind:n to:n :}
   N-ADDRS @ {: k:n :}
   k 1+ ADDR-AT-RESERVE  k 1+ ADDR-KIND-AT-RESERVE  k 1+ ADDR-TO-RESERVE
   at k ADDR-AT !  kind k ADDR-KIND-AT !  to k ADDR-TO !
   k 1+ N-ADDRS ! ;

\ The rows read back, for an emitter's PATCH: unit k's start, and call row and
\ address row k as ( at kind target ).
: UNIT ( n -- n ) UNIT-AT @ ;
: CALL-ROWS ( -- n ) N-CALLS @ ;
: CALL-ROW ( n -- n n n ) {: k:n :}  k CALL-AT @  k CALL-KIND-AT @  k CALL-TO @ ;
: ADDR-ROWS ( -- n ) N-ADDRS @ ;
: ADDR-ROW ( n -- n n n ) {: k:n :}  k ADDR-AT @  k ADDR-KIND-AT @  k ADDR-TO @ ;

\ How many vregs the selected emitter has.
ARCHES TYPED-BUFFER VREGS-N n
: VREGS ( -- n ) CUR @ VREGS-N @ ;

\ ---- the body ----------------------------------------------------------------
ARCHES TYPED-BUFFER OPEN-XT [ -- ]
ARCHES TYPED-BUFFER QUOT-OPEN-XT [ n -- ]
ARCHES TYPED-BUFFER RETURN-XT [ -- ]
ARCHES TYPED-BUFFER CLOSE-XT [ -- ]
ARCHES TYPED-BUFFER BYTES-XT [ -- ptr u8 n ]
ARCHES TYPED-BUFFER PATCH-XT [ n -- ]

private

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
: QUOT-OPEN ( n -- ) CUR @ QUOT-OPEN-XT @ execute ;

\ Leave the word, from the middle of its body or at its end.
: RETURN ( -- ) CUR @ RETURN-XT @ execute ;

: CLOSE ( -- ) CUR @ CLOSE-XT @ execute ;

\ ---- values ------------------------------------------------------------------
ARCHES TYPED-BUFFER LIT-XT [ n vreg -- ]
ARCHES TYPED-BUFFER ADDR-DATA-XT [ n vreg -- ]
ARCHES TYPED-BUFFER ADDR-CODE-XT [ n vreg -- ]
ARCHES TYPED-BUFFER ADDR-QUOT-XT [ n vreg -- ]
ARCHES TYPED-BUFFER PUSH-XT [ vreg -- ]
ARCHES TYPED-BUFFER SPILL-XT [ vreg n -- ]
ARCHES TYPED-BUFFER SPILL-LIT-XT [ n n -- ]
ARCHES TYPED-BUFFER FILL-XT [ vreg n -- ]
ARCHES TYPED-BUFFER DSP+-XT [ n -- ]
ARCHES TYPED-BUFFER MOV-XT [ vreg vreg -- ]
ARCHES TYPED-BUFFER SLOTS-XT [ n -- ]
ARCHES TYPED-BUFFER SLOT-STORE-XT [ vreg n -- ]
ARCHES TYPED-BUFFER SLOT-LOAD-XT [ vreg n -- ]

: LIT ( n vreg -- ) CUR @ LIT-XT @ execute ;
: ADDR-DATA ( n vreg -- ) CUR @ ADDR-DATA-XT @ execute ;
: ADDR-CODE ( n vreg -- ) CUR @ ADDR-CODE-XT @ execute ;

\ The address of quotation unit n, which QUOT-OPEN begins later.
: ADDR-QUOT ( n vreg -- ) CUR @ ADDR-QUOT-XT @ execute ;

\ The data stack grows up: a push stores at the stack pointer and adds a cell,
\ a pop subtracts a cell and loads. PUSH pushes one cell; SPILL,
\ SPILL-LIT and FILL address the cell k cells above the stack pointer without
\ moving it, and DSP+ moves it by k cells.
: PUSH ( vreg -- ) CUR @ PUSH-XT @ execute ;
: SPILL ( vreg n -- ) CUR @ SPILL-XT @ execute ;
: SPILL-LIT ( n n -- ) CUR @ SPILL-LIT-XT @ execute ;
: FILL ( vreg n -- ) CUR @ FILL-XT @ execute ;
: DSP+ ( n -- ) CUR @ DSP+-XT @ execute ;
: MOV ( vreg vreg -- ) CUR @ MOV-XT @ execute ;

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
ARCHES TYPED-BUFFER NEG-XT [ vreg -- ]
ARCHES TYPED-BUFFER NOT-XT [ vreg -- ]
ARCHES TYPED-BUFFER CMP-SET-XT [ vreg vreg cc -- ]
ARCHES TYPED-BUFFER CMP-SET-LIT-XT [ vreg n cc -- ]
ARCHES TYPED-BUFFER IMM?-XT [ n -- bool ]

\ Whether OP-LIT, MUL-LIT, CMP-SET-LIT and OF-LIT take n as their immediate;
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
: NEG ( vreg -- ) CUR @ NEG-XT @ execute ;
: NOT ( vreg -- ) CUR @ NOT-XT @ execute ;

\ d becomes -1 when d c s (or d c n) holds, else 0: a Habu flag.
: CMP-SET ( vreg vreg cc -- ) CUR @ CMP-SET-XT @ execute ;
: CMP-SET-LIT ( vreg n cc -- ) CUR @ CMP-SET-LIT-XT @ execute ;

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
ARCHES TYPED-BUFFER IF,-XT [ vreg -- ]
ARCHES TYPED-BUFFER ELSE,-XT [ -- ]
ARCHES TYPED-BUFFER THEN,-XT [ -- ]
ARCHES TYPED-BUFFER BEGIN,-XT [ -- ]
ARCHES TYPED-BUFFER WHILE,-XT [ vreg -- ]
ARCHES TYPED-BUFFER REPEAT,-XT [ -- ]
ARCHES TYPED-BUFFER UNTIL,-XT [ vreg -- ]
ARCHES TYPED-BUFFER AGAIN,-XT [ -- ]
ARCHES TYPED-BUFFER CASE,-XT [ -- ]
ARCHES TYPED-BUFFER OF,-XT [ vreg vreg -- ]
ARCHES TYPED-BUFFER OF-LIT-XT [ vreg n -- ]
ARCHES TYPED-BUFFER ENDOF,-XT [ -- ]
ARCHES TYPED-BUFFER ENDCASE,-XT [ -- ]

\ `if`: past the arm when the vreg is zero.
: IF, ( vreg -- ) CUR @ IF,-XT @ execute ;

\ `else`: past the second arm, then the open `if` lands here.
: ELSE, ( -- ) CUR @ ELSE,-XT @ execute ;

\ `then`: the innermost open forward branch lands here.
: THEN, ( -- ) CUR @ THEN,-XT @ execute ;

: BEGIN, ( -- ) CUR @ BEGIN,-XT @ execute ;

\ `while`: out of the loop when the vreg is zero; the exit stays open beneath
\ the head, as Forth orders orig and dest, so `begin ... while ... while ...
\ repeat then` resolves.
: WHILE, ( vreg -- ) CUR @ WHILE,-XT @ execute ;

\ `repeat`: back to the head, then the innermost `while` exit lands here.
: REPEAT, ( -- ) CUR @ REPEAT,-XT @ execute ;

\ `until`: back to the head when the vreg is zero.
: UNTIL, ( vreg -- ) CUR @ UNTIL,-XT @ execute ;

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
ARCHES TYPED-BUFFER TOR-XT [ vreg -- ]
ARCHES TYPED-BUFFER RFROM-XT [ vreg -- ]
ARCHES TYPED-BUFFER RFETCH-XT [ vreg -- ]

\ `>r`, `r>` and `r@` move a cell between a vreg and the return stack, which
\ is RSP-CELL cells deep from STACK-ABI:RETURN-BASE-CELL (src/habu/layout.f
\ RSP-CELL).
: TOR ( vreg -- ) CUR @ TOR-XT @ execute ;
: RFROM ( vreg -- ) CUR @ RFROM-XT @ execute ;
: RFETCH ( vreg -- ) CUR @ RFETCH-XT @ execute ;

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
\ LOOP-CLOSE lands its exits on the pop of its frame.

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
\ the limit. t is a free vreg it clobbers.
: LOOP-NEXT ( vreg -- ) CUR @ LOOP-NEXT-XT @ execute ;

\ `+loop`: step the index by s and branch back to the head unless it crossed
\ the boundary. s and t are clobbered; t is a free vreg. A `?do` entry of the
\ level now skips equal bounds only.
: LOOP-STEP ( vreg vreg -- ) CUR @ LOOP-STEP-XT @ execute ;

\ Land the innermost level's `leave` exits and entry here, close it and pop
\ its frame.
: LOOP-CLOSE ( -- ) CUR @ LOOP-CLOSE-XT @ execute ;

\ ---- the registry ------------------------------------------------------------
\ Run an architecture's installer once, with the architecture's slot, and let
\ SELECT choose it.
: PROVIDE ( CTARGET:arch [ n -- ] -- )
   swap SLOT {: k:n :}
   k swap execute
   true k INSTALLED ! ;

\ Make the architecture's emitter the one the operations run.
: SELECT ( CTARGET:arch -- )
   {: a:CTARGET:arch :}
   a SLOT {: k:n :}
   k INSTALLED @ 0= if E-T0-TARGET throw then
   k CUR !
   a CUR-ARCH ! ;

\ ---- sealing -----------------------------------------------------------------
private

: NEMIT-KIND ( n -- n ) {: kind:n :}
   kind ADDR-QUOT-KIND = if NEMIT:ADDR-CODE exit then
   kind ;

: NEMIT-OPEN ( -- ) BYTES 0 CUR-ARCH @ NEMIT:OPEN ;

: NEMIT-ROWS ( -- )
   N-UNITS @ 0 ?do i UNIT-AT @ NEMIT:FUNCTION+ loop
   N-CALLS @ 0 ?do
      i CALL-AT @  i CALL-KIND-AT @  i CALL-TO @  0  i CALL-LOC @  NEMIT:CALL-SITE+
   loop
   N-ADDRS @ 0 ?do
      i ADDR-AT @  i ADDR-KIND-AT @ NEMIT-KIND  -1  NEMIT:ADDR-SITE+
   loop
   NEMIT:SEAL ;

public

\ Seal the closed emission into NEMIT, placed at address n.
: SEAL ( n -- ) {: at:n :}
   NEMIT-OPEN
   at PATCH
   at NEMIT:PLACE
   NEMIT-ROWS ;

\ Seal it with no placement, the bytes as emitted.
: SEAL-UNPLACED ( -- )
   NEMIT-OPEN
   NEMIT-ROWS ;

;package

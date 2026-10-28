\ t0-emit.f - the emitter seam of the tier 0 compiler, package T0-EMIT.
\
\ src/habu/t0.f (package T0) walks a definition's body text and drives the
\ operations below; an emitter file per target implements them
\ (src/habu/t0-x64.f for x86-64). The front end names values by virtual
\ register (vreg 0..VREGS-1) and never by machine register, so it stays the
\ same for every target, and the types, constants and rules here mean the same
\ on every target. An emitter file states only its encodings and register
\ choices.
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
\ THE SINK. OPEN begins an emission, emptying the emitter's sink; CLOSE ends
\ a unit, and once the last unit is closed CODE$ reads the finished emission
\ until the next OPEN begins the next.
\
\ THE UNITS. A definition and its quotations are one emission: unit 0 is the
\ definition and starts at byte 0, and each quotation is a later unit, a
\ function of its own that QUOT-OPEN begins where the previous unit's CLOSE
\ left off. The publisher places CODE$ whole and finds each quotation through
\ its ADDR-QUOT-KIND row. `recurse` in any unit calls byte 0, the definition,
\ as both tiers run it.
\
\ THE FRAME. Each unit has a frame on the machine stack that begins with no
\ cells; SLOTS raises it to cover the slots the front end uses. The front end
\ ends every unit with RETURN or TAIL, each of which drops the frame, and
\ CLOSE then fixes the unit's frame size.
\
\ THE ROWS. Every call or tail site and every code, data or quotation address
\ site is recorded for the publisher to place and relocate: its offset, which
\ is the instruction's first byte, its kind and its target. CALL-ROWS /
\ CALL-ROW and ADDR-ROWS / ADDR-ROW read them as ( at kind target ). Offsets
\ are from the emission's byte 0, and each unit's rows follow the previous
\ unit's. A quotation's address (ADDR-QUOT-KIND) is a code address inside the
\ emission: its target, and the address the instruction holds, is the
\ quotation unit's offset, which QUOT-OPEN fills in; the publisher adds the
\ placement.
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

\ The kinds of a call row and of an address row.
0 constant CALL-KIND
1 constant TAIL-KIND
1 constant ADDR-DATA-KIND
2 constant ADDR-CODE-KIND
3 constant ADDR-QUOT-KIND

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

public

\ How many vregs the selected emitter has.
ARCHES TYPED-BUFFER VREGS-N n
: VREGS ( -- n ) CUR @ VREGS-N @ ;

\ ---- the body ----------------------------------------------------------------
ARCHES TYPED-BUFFER OPEN-XT [ -- ]
ARCHES TYPED-BUFFER QUOT-OPEN-XT [ n -- ]
ARCHES TYPED-BUFFER RETURN-XT [ -- ]
ARCHES TYPED-BUFFER CLOSE-XT [ -- ]
ARCHES TYPED-BUFFER CODE$-XT [ -- ptr u8 n ]
ARCHES TYPED-BUFFER CALL-ROWS-XT [ -- n ]
ARCHES TYPED-BUFFER CALL-ROW-XT [ n -- n n n ]
ARCHES TYPED-BUFFER ADDR-ROWS-XT [ -- n ]
ARCHES TYPED-BUFFER ADDR-ROW-XT [ n -- n n n ]

: OPEN ( -- ) CUR @ OPEN-XT @ execute ;

\ Begin quotation unit n, which ADDR-QUOT named, and bind that address row to
\ it. Units begin in order, so n is the count begun so far.
: QUOT-OPEN ( n -- ) CUR @ QUOT-OPEN-XT @ execute ;

\ Leave the word, from the middle of its body or at its end.
: RETURN ( -- ) CUR @ RETURN-XT @ execute ;

: CLOSE ( -- ) CUR @ CLOSE-XT @ execute ;
: CODE$ ( -- ptr u8 n ) CUR @ CODE$-XT @ execute ;
: CALL-ROWS ( -- n ) CUR @ CALL-ROWS-XT @ execute ;
: CALL-ROW ( n -- n n n ) CUR @ CALL-ROW-XT @ execute ;
: ADDR-ROWS ( -- n ) CUR @ ADDR-ROWS-XT @ execute ;
: ADDR-ROW ( n -- n n n ) CUR @ ADDR-ROW-XT @ execute ;

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
ARCHES TYPED-BUFFER CALL-XT [ n -- ]
ARCHES TYPED-BUFFER TAIL-XT [ n -- ]
ARCHES TYPED-BUFFER CALL-SELF-XT [ -- ]

: CALL ( n -- ) CUR @ CALL-XT @ execute ;
: TAIL ( n -- ) CUR @ TAIL-XT @ execute ;

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
   SLOT {: k:n :}
   k INSTALLED @ 0= if E-T0-TARGET throw then
   k CUR ! ;

;package

split-str
\ t0-emit.f - the emitter seam of the tier 0 compiler, package COMP.
\
\ src/habu/t0.f drives the EMIT- operations below; an emitter part per target
\ implements them (ARM64 in src/habu/t0-arm64.f, X86-64 in src/habu/t0-x64.f,
\ WASM in src/habu/t0-wasm.f).
\ The front end names values by vreg (0 .. EMIT-VREGS - 1), never by machine
\ register, so the types, constants and rules here hold on every target.

require src/lib/error.f
require src/lib/type/deftype.f
require src/compiler/target.f
require src/compiler/native/emission.f
require src/compiler/native/callback-stub.f

package COMP

-9590 constant E-FIRST
-9599 constant E-LAST
-9590 s" the architecture EMIT-SELECT was given has no emitter" ERROR:CODE E-T0-TARGET
-9591 s" EMIT-QUOT-OPEN opened a unit other than the next, EMIT-REC-CELLS or EMIT-ENV-OPEN ran after the unit's first instruction, or EMIT-ENV-LOAD ran in a unit EMIT-ENV-OPEN did not open"
   ERROR:CODE E-T0-UNIT
-9592 s" the selected emitter does not provide the operation" ERROR:FAULT E-T0-UNFILLED
-9593 s" code was emitted after the does> patch slot" ERROR:CODE E-T0-SLOT
-9594 s" a vreg, operation, shift or condition is outside its type" ERROR:CODE E-T0-OPERAND

DEFTYPE VREG
DEFTYPE ALU
DEFTYPE SHIFTER
DEFTYPE CC
DEFTYPE WIDTH

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

NEMIT:ADDR-KIND-MAX 1+ constant ADDR-QUOT-KIND

\ ---- the architectures -------------------------------------------------------
7 constant ARCHES

ARCHES TYPED-BUFFER INSTALLED bool

: ARCH>INDEX ( CTARGET:arch -- n )
   MATCH CTARGET:arch
      aarch64 OF 0 ENDOF
      ptx     OF 1 ENDOF
      a32     OF 2 ENDOF
      thumb2  OF 3 ENDOF
      c66x    OF 4 ENDOF
      x86-64  OF 5 ENDOF
      wasm    OF 6 ENDOF
   ;MATCH ;

\ The selected architecture's slot; -1, no slot, until EMIT-SELECT.
variable CUR
-1 CUR !
TYPED-VARIABLE CUR-ARCH CTARGET:arch

: EMIT-SELECTED ( -- n ) CUR @ ;

\ ---- the rows ----------------------------------------------------------------
\ THE ROWS. EMIT-CALL-ROW+ files a call or tail site ( at kind target loc ),
\ kind NEMIT:CALL or NEMIT:TAIL, loc the calling token's byte offset in the body
\ text. EMIT-ADDR-ROW+ files an address site ( at kind target origin ), kind
\ NEMIT:ADDR-DATA, NEMIT:ADDR-CODE or ADDR-QUOT-KIND; a data address's origin
\ comes with it through EMIT-ADDR-DATA, and a code or quotation address's is
\ -1. at is the instruction's first byte from the emission's byte 0. A
\ quotation's target is its unit number. An emitter whose quotation address
\ is relative to its own site (arm64's `adr`, x86-64's RIP-relative `lea`)
\ files no row for it.
variable N-UNITS
DYNAMIC-BUFFER UNIT-AT n
variable N-CALLS
DYNAMIC-BUFFER CALL-AT n
DYNAMIC-BUFFER CALL-KIND-AT n
DYNAMIC-BUFFER CALL-TO n
DYNAMIC-BUFFER CALL-LOC n
variable N-ADDRS
DYNAMIC-BUFFER ADDR-AT n
DYNAMIC-BUFFER ADDR-KIND-AT n
DYNAMIC-BUFFER ADDR-TO n
DYNAMIC-BUFFER ADDR-ORIGIN n
variable SLOT-AT                      \ the patch slot's offset, or -1
variable STUB-AT                      \ the callback stub's offset once stamped, or -1

\ The emission's phase: shut from EMIT-SEAL to EMIT-OPEN, open from EMIT-OPEN,
\ placed from EMIT-PLACE; only these three words write it.
ENUM emission shut open placed ;ENUM
TYPED-VARIABLE PHASE emission
variable PLACED-AT

: ROWS-CLEAR ( -- )
   0 N-UNITS !  0 N-CALLS !  0 N-ADDRS !  -1 SLOT-AT !  -1 STUB-AT ! ;
: SLOT? ( -- bool ) SLOT-AT @ 0 >= ;

: EMIT-UNIT+ ( n -- ) {: at:n :}
   N-UNITS @ {: k:n :}
   k 1+ UNIT-AT-RESERVE
   at k UNIT-AT !
   k 1+ N-UNITS ! ;

: EMIT-CALL-ROW+ ( n n n n -- ) {: at:n kind:n to:n loc:n :}
   N-CALLS @ {: k:n :}
   k 1+ CALL-AT-RESERVE  k 1+ CALL-KIND-AT-RESERVE
   k 1+ CALL-TO-RESERVE  k 1+ CALL-LOC-RESERVE
   at k CALL-AT !  kind k CALL-KIND-AT !  to k CALL-TO !  loc k CALL-LOC !
   k 1+ N-CALLS ! ;

: EMIT-ADDR-ROW+ ( n n n n -- ) {: at:n kind:n to:n origin:n :}
   N-ADDRS @ {: k:n :}
   k 1+ ADDR-AT-RESERVE  k 1+ ADDR-KIND-AT-RESERVE  k 1+ ADDR-TO-RESERVE
   k 1+ ADDR-ORIGIN-RESERVE
   at k ADDR-AT !  kind k ADDR-KIND-AT !  to k ADDR-TO !  origin k ADDR-ORIGIN !
   k 1+ N-ADDRS ! ;

: EMIT-PATCH-SLOT! ( n -- ) SLOT-AT ! ;

: EMIT-UNIT ( n -- n ) UNIT-AT @ ;
: EMIT-CALL-ROWS ( -- n ) N-CALLS @ ;
: EMIT-CALL-ROW ( n -- n n n ) {: k:n :}
   k CALL-AT @  k CALL-KIND-AT @  k CALL-TO @ ;
: EMIT-ADDR-ROWS ( -- n ) N-ADDRS @ ;
: EMIT-ADDR-ROW ( n -- n n n ) {: k:n :}
   k ADDR-AT @  k ADDR-KIND-AT @  k ADDR-TO @ ;

ARCHES TYPED-BUFFER EMIT-VREGS-N n
: EMIT-VREGS ( -- n ) CUR @ EMIT-VREGS-N @ ;

\ ---- the cells vregs hold ------------------------------------------------------
\ A vreg is unknown or holds a cell, allocated or free: frame slot k, whose key is
\ k, or cell n of the record the unit was called with, whose key is n invert.
\ EMIT-SLOT-STORE, EMIT-SLOT-LOAD and EMIT-ENV-LOAD: the vreg holds the cell, and
\ the vreg that held it is unknown. Any other EMIT- word that writes a vreg makes
\ it unknown. A call, a tail, a return, a loop's head, a counted loop's exit and a
\ unit's start: every vreg is unknown; a branch's fallthrough changes none.
\
\ These are the facts. Any other label's facts are those all its live arrivals
\ share: a branch's are the facts where it leaves, which a row keeps, and no
\ emitter writes a vreg at a label. EMIT-ELSE, EMIT-THEN, EMIT-REPEAT,
\ EMIT-ENDOF, and EMIT-ENDCASE, leave the facts to their caller.
variable CACHED                       \ bit v set: vreg v holds the cell v CACHE-KEY
DYNAMIC-BUFFER CACHE-KEY n            \ reserved to EMIT-VREGS by EMIT-OPEN
DYNAMIC-BUFFER CACHE-MASK n           \ row k's CACHED, or NO-ARRIVAL
DYNAMIC-BUFFER CACHE-KEYS n           \ row k's CACHE-KEY from k*EMIT-VREGS
-1 constant NO-ARRIVAL                \ a row no arrival has reached

: CACHE-CLEAR ( -- ) 0 CACHED ! ;

: CACHE-DROP ( vreg -- )
   1 swap VREG>N lshift invert CACHED @ and CACHED ! ;

\ The vreg that holds the cell of key k, or -1; no two hold the same one.
: KEY-VREG ( n -- n ) {: k:n :}
   0 CACHED @ begin dup 0<> while
      dup 1 and 0<> if over CACHE-KEY @ k = if drop exit then then
      1 rshift swap 1+ swap
   repeat 2drop -1 ;

: CACHE-FILE ( vreg n -- ) {: v:vreg k:n :}
   k KEY-VREG dup 0< if drop else >VREG CACHE-DROP then
   k v VREG>N CACHE-KEY !
   1 v VREG>N lshift CACHED @ or CACHED ! ;

: CACHE-ROOM ( n -- ) {: k:n :}
   k 1+ CACHE-MASK-RESERVE  k 1+ EMIT-VREGS * CACHE-KEYS-RESERVE ;

: CACHE-VOID ( n -- ) {: k:n :} NO-ARRIVAL k CACHE-MASK ! ;

: KEPT-KEY ( n n -- ^n ) {: k:n v:n :} k EMIT-VREGS * v + CACHE-KEYS ;

: CACHE-KEEP ( n -- ) {: k:n :}
   CACHED @ k CACHE-MASK !
   EMIT-VREGS 0 ?do i CACHE-KEY @ k i KEPT-KEY ! loop ;

\ The facts are row k's, none without an arrival.
: CACHE-TAKE ( n -- ) {: k:n :}
   k CACHE-MASK @ 0 max CACHED !
   EMIT-VREGS 0 ?do k i KEPT-KEY @ i CACHE-KEY ! loop ;

\ The facts are those row k shares; one without an arrival adds no limit.
: CACHE-MEET ( n -- ) {: k:n :}
   k CACHE-MASK @ {: m:n :}
   m NO-ARRIVAL = if exit then
   m CACHED @ and CACHED !
   EMIT-VREGS 0 ?do
      i CACHE-KEY @ k i KEPT-KEY @ <> if i >VREG CACHE-DROP then
   loop ;

: CACHE-COPY ( n n -- ) {: from:n to:n :}
   from CACHE-MASK @ to CACHE-MASK !
   EMIT-VREGS 0 ?do from i KEPT-KEY @ to i KEPT-KEY ! loop ;

\ ---- the body ----------------------------------------------------------------
\ THE SINK. EMIT-OPEN empties the emitter's sink and the seam's rows; EMIT-CLOSE
\ ends a unit; EMIT-SEAL hands the emission to NEMIT. The caller runs
\ NEMIT:CLEAR before the next EMIT-OPEN, on the accepting and the rejecting path
\ alike. NEMIT reads the bytes in place, so they stay the emitter's until the
\ next EMIT-OPEN.
\
\ THE UNITS. A definition and its quotations are one emission: unit 0 is the
\ definition at byte 0, and each quotation is a later unit that EMIT-QUOT-OPEN
\ begins where the previous EMIT-CLOSE left off. A `does>` clause is a later
\ unit too, begun by EMIT-QUOT-OPEN when the parent pushed its address, else by
\ EMIT-UNIT-OPEN. Sealing files one NEMIT function per unit. `recurse` in any
\ unit calls byte 0 and files no row.
\
\ THE PATCH SLOT. EMIT-SLOT-CLOSE closes a unit whose return is the does> patch
\ slot (EMIT-PATCH-SLOT!). The kernel finds the slot from the record's length,
\ so the slot ends the emission: a unit begun after it, or a callback stub with
\ it, throws E-T0-SLOT.
ARCHES TYPED-BUFFER EMIT-OPEN-XT [ -- ]
ARCHES TYPED-BUFFER EMIT-QUOT-OPEN-XT [ n -- ]
ARCHES TYPED-BUFFER EMIT-UNIT-OPEN-XT [ -- ]
ARCHES TYPED-BUFFER EMIT-RETURN-XT [ -- ]
ARCHES TYPED-BUFFER EMIT-CLOSE-XT [ -- ]
ARCHES TYPED-BUFFER EMIT-BYTES-XT [ -- str ]
ARCHES TYPED-BUFFER EMIT-PATCH-XT [ n -- ]
ARCHES TYPED-BUFFER EMIT-SLOT-CLOSE-XT [ -- ]
ARCHES TYPED-BUFFER EMIT-CB-STUB-XT [ -- n ]

\ An xt as the number its cell holds: 0 while unset.
CAST: CALLABLE>N ( [ R -- S ] -- n )

: FILLED ( [ R -- S ] -- [ R -- S ] )
   dup CALLABLE>N 0= if E-T0-UNFILLED throw then ;

\ The finished emission, once the last unit is closed.
: EMIT-BYTES ( -- str ) CUR @ EMIT-BYTES-XT @ execute ;

\ Write every call row's displacement and every quotation address row for the
\ emission placed at address n.
: EMIT-PATCH ( n -- ) CUR @ EMIT-PATCH-XT @ execute ;

: EMIT-OPEN ( -- )
   ROWS-CLEAR  0 PLACED-AT !  construct emission open PHASE !
   EMIT-VREGS CACHE-KEY-RESERVE  CACHE-CLEAR
   CUR @ EMIT-OPEN-XT @ execute ;

\ Begin quotation unit n, which EMIT-ADDR-QUOT named, and bind the address
\ EMIT-ADDR-QUOT emitted to it. Units begin in order, so n is the count begun so
\ far.
: EMIT-QUOT-OPEN ( n -- )
   SLOT? if E-T0-SLOT throw then
   CACHE-CLEAR
   CUR @ EMIT-QUOT-OPEN-XT @ execute ;

\ Begin the next unit with no address bound to it, for a does> definer's
\ empty clause, whose parent pushes no clause address.
: EMIT-UNIT-OPEN ( -- )
   SLOT? if E-T0-SLOT throw then
   CACHE-CLEAR
   CUR @ EMIT-UNIT-OPEN-XT @ FILLED execute ;

\ Leave the word, from the middle of its body or at its end.
: EMIT-RETURN ( -- ) CACHE-CLEAR  CUR @ EMIT-RETURN-XT @ execute ;

: EMIT-CLOSE ( -- ) CUR @ EMIT-CLOSE-XT @ execute ;

\ Leave the word by a return that is the does> patch slot, and close the unit
\ (THE PATCH SLOT).
: EMIT-SLOT-CLOSE ( -- ) CUR @ EMIT-SLOT-CLOSE-XT @ FILLED execute ;

\ ---- values ------------------------------------------------------------------
ARCHES TYPED-BUFFER EMIT-LIT-XT [ n vreg -- ]
ARCHES TYPED-BUFFER EMIT-ADDR-DATA-XT [ n n vreg -- ]
ARCHES TYPED-BUFFER EMIT-ADDR-CODE-XT [ n vreg -- ]
ARCHES TYPED-BUFFER EMIT-ADDR-QUOT-XT [ n vreg -- ]
ARCHES TYPED-BUFFER EMIT-PUSH-XT [ vreg n -- ]
ARCHES TYPED-BUFFER EMIT-PUSH-LIT-XT [ n n -- ]
ARCHES TYPED-BUFFER EMIT-POP-XT [ vreg n -- ]
ARCHES TYPED-BUFFER EMIT-SPILL-XT [ vreg n -- ]
ARCHES TYPED-BUFFER EMIT-SPILL-LIT-XT [ n n -- ]
ARCHES TYPED-BUFFER EMIT-FILL-XT [ vreg n -- ]
ARCHES TYPED-BUFFER EMIT-DSP+-XT [ n -- ]
ARCHES TYPED-BUFFER EMIT-MOV-XT [ vreg vreg -- ]
ARCHES TYPED-BUFFER EMIT-EXCHANGE-XT [ vreg vreg -- ]
ARCHES TYPED-BUFFER EMIT-SLOTS-XT [ n -- ]
ARCHES TYPED-BUFFER EMIT-SLOT-STORE-XT [ vreg n -- ]
ARCHES TYPED-BUFFER EMIT-SLOT-LOAD-XT [ vreg n -- ]

: EMIT-LIT ( n vreg -- ) dup CACHE-DROP  CUR @ EMIT-LIT-XT @ execute ;
\ Data address v, whose address row files origin o (THE ROWS).
: EMIT-ADDR-DATA ( n n vreg -- )
   dup CACHE-DROP  CUR @ EMIT-ADDR-DATA-XT @ execute ;
: EMIT-ADDR-CODE ( n vreg -- )
   dup CACHE-DROP  CUR @ EMIT-ADDR-CODE-XT @ execute ;

\ The address of quotation unit n, which EMIT-QUOT-OPEN begins later.
: EMIT-ADDR-QUOT ( n vreg -- )
   dup CACHE-DROP  CUR @ EMIT-ADDR-QUOT-XT @ execute ;

\ The data stack grows up. EMIT-SPILL, EMIT-SPILL-LIT and EMIT-FILL address the
\ cell k cells above the stack pointer without moving it; EMIT-DSP+ moves it by
\ k cells. EMIT-PUSH and EMIT-PUSH-LIT store at the pointer then move it up k
\ cells, and EMIT-POP moves it down k cells then loads: with k 1 a push and a
\ pop, otherwise the last transfer of a k-cell spill or fill, so the move costs
\ no instruction of its own where the target writes back.
: EMIT-PUSH ( vreg n -- ) CUR @ EMIT-PUSH-XT @ execute ;
: EMIT-PUSH-LIT ( n n -- ) CUR @ EMIT-PUSH-LIT-XT @ execute ;
: EMIT-POP ( vreg n -- ) over CACHE-DROP  CUR @ EMIT-POP-XT @ execute ;
: EMIT-SPILL ( vreg n -- ) CUR @ EMIT-SPILL-XT @ execute ;
: EMIT-SPILL-LIT ( n n -- ) CUR @ EMIT-SPILL-LIT-XT @ execute ;
: EMIT-FILL ( vreg n -- )
   over CACHE-DROP  CUR @ EMIT-FILL-XT @ execute ;
: EMIT-DSP+ ( n -- ) CUR @ EMIT-DSP+-XT @ execute ;
: EMIT-MOV ( vreg vreg -- )
   over CACHE-DROP  CUR @ EMIT-MOV-XT @ execute ;
\ The two vregs' values swapped, a join's register cycle.
: EMIT-EXCHANGE ( vreg vreg -- )
   2dup CACHE-DROP CACHE-DROP  CUR @ EMIT-EXCHANGE-XT @ execute ;

\ The frame: EMIT-SLOTS makes it hold at least n cells, EMIT-SLOT-STORE and
\ EMIT-SLOT-LOAD move a cell between a vreg and slot k.
: EMIT-SLOTS ( n -- ) CUR @ EMIT-SLOTS-XT @ execute ;
: EMIT-SLOT-STORE ( vreg n -- )
   2dup CACHE-FILE  CUR @ EMIT-SLOT-STORE-XT @ execute ;
: EMIT-SLOT-LOAD ( vreg n -- )
   2dup CACHE-FILE  CUR @ EMIT-SLOT-LOAD-XT @ execute ;

\ A reading quotation's record lies in the frame of the unit it is written in, on Wasm in
\ return-stack cells that unit takes: cell 0 its unit's code, then the locals. Its value
\ is the address with bit 63 set.
ARCHES TYPED-BUFFER EMIT-REC-CELLS-XT [ n -- ]
ARCHES TYPED-BUFFER EMIT-ENV-OPEN-XT [ -- ]
ARCHES TYPED-BUFFER EMIT-REC-STORE-XT [ vreg n -- ]
ARCHES TYPED-BUFFER EMIT-REC-QUOT-XT [ n vreg -- ]
ARCHES TYPED-BUFFER EMIT-ENV-LOAD-XT [ vreg n -- ]

\ The open unit's frame holds n record cells; before its first instruction.
: EMIT-REC-CELLS ( n -- ) CUR @ EMIT-REC-CELLS-XT @ FILLED execute ;

\ The open unit is a reading quotation and keeps the record's address it was
\ called with; its first instruction, after EMIT-REC-CELLS when both are called.
: EMIT-ENV-OPEN ( -- ) CUR @ EMIT-ENV-OPEN-XT @ FILLED execute ;

\ Store the vreg into record cell n of the open unit.
: EMIT-REC-STORE ( vreg n -- ) CUR @ EMIT-REC-STORE-XT @ FILLED execute ;

\ The vreg becomes the quotation value of the record starting at record cell n.
: EMIT-REC-QUOT ( n vreg -- )
   dup CACHE-DROP  CUR @ EMIT-REC-QUOT-XT @ FILLED execute ;

\ The vreg becomes cell n of the record the open unit was called with; cell 0
\ is its code address.
: EMIT-ENV-LOAD ( vreg n -- )
   2dup invert CACHE-FILE  CUR @ EMIT-ENV-LOAD-XT @ FILLED execute ;

\ ---- arithmetic --------------------------------------------------------------
ARCHES TYPED-BUFFER EMIT-OP-XT [ vreg vreg alu -- ]
ARCHES TYPED-BUFFER EMIT-OP-LIT-XT [ vreg n alu -- ]
ARCHES TYPED-BUFFER EMIT-SHIFT-XT [ vreg vreg shifter -- ]
ARCHES TYPED-BUFFER EMIT-SHIFT-LIT-XT [ vreg n shifter -- ]
ARCHES TYPED-BUFFER EMIT-MULX-XT [ vreg vreg -- ]
ARCHES TYPED-BUFFER EMIT-MUL-LIT-XT [ vreg n -- ]
ARCHES TYPED-BUFFER EMIT-DIV-LIT-XT [ vreg n -- ]
ARCHES TYPED-BUFFER EMIT-MOD-LIT-XT [ vreg n -- ]
ARCHES TYPED-BUFFER EMIT-NEG-XT [ vreg -- ]
ARCHES TYPED-BUFFER EMIT-NOT-XT [ vreg -- ]
ARCHES TYPED-BUFFER EMIT-CMP-XT [ vreg vreg -- ]
ARCHES TYPED-BUFFER EMIT-CMP-LIT-XT [ vreg n -- ]
ARCHES TYPED-BUFFER EMIT-FLAG-XT [ vreg vreg vreg cc -- ]
ARCHES TYPED-BUFFER EMIT-FLAG-LIT-XT [ vreg vreg n cc -- ]
ARCHES TYPED-BUFFER EMIT-IMM?-XT [ n -- bool ]

\ Whether EMIT-OP-LIT, EMIT-MUL-LIT, EMIT-CMP-LIT, EMIT-FLAG-LIT and EMIT-OF-LIT
\ take n as their immediate; each of them requires n to answer true.
: EMIT-IMM? ( n -- bool ) CUR @ EMIT-IMM?-XT @ execute ;

: EMIT-OP ( vreg vreg alu -- ) {: d:vreg s:vreg o:alu :}
   d CACHE-DROP  d s o CUR @ EMIT-OP-XT @ execute ;
: EMIT-OP-LIT ( vreg n alu -- ) {: d:vreg v:n o:alu :}
   d CACHE-DROP  d v o CUR @ EMIT-OP-LIT-XT @ execute ;

\ A shift count is masked to six bits, as Habu's `lshift` and `rshift` are (a
\ count of 64 shifts by nothing); SH-RIGHT is logical.
: EMIT-SHIFT ( vreg vreg shifter -- ) {: d:vreg s:vreg o:shifter :}
   d CACHE-DROP  d s o CUR @ EMIT-SHIFT-XT @ execute ;
: EMIT-SHIFT-LIT ( vreg n shifter -- ) {: d:vreg v:n o:shifter :}
   d CACHE-DROP  d v o CUR @ EMIT-SHIFT-LIT-XT @ execute ;

: EMIT-MULX ( vreg vreg -- )
   over CACHE-DROP  CUR @ EMIT-MULX-XT @ execute ;
: EMIT-MUL-LIT ( vreg n -- )
   over CACHE-DROP  CUR @ EMIT-MUL-LIT-XT @ execute ;

\ DIVISION. `/` and `mod` by a literal divisor other than 0 and -1 are
\ EMIT-DIV-LIT and EMIT-MOD-LIT. A zero divisor stays a call of the kernel row,
\ so no fold divides by it on the host; -1 stays a call because x86-64 idiv
\ faults on MIN-N -1 (arm64 sdiv answers MIN-N), and one rule holds for every
\ target. A register divisor and `/mod` stay calls too.
\
\ EMIT-DIV-LIT and EMIT-MOD-LIT replace the dividend in d with its quotient by
\ n, truncated toward zero, or the remainder that carries the dividend's sign,
\ as the kernel's `/` and `mod` rows compute them; n is neither 0 nor -1. Every
\ other vreg keeps its value.
: EMIT-DIV-LIT ( vreg n -- )
   over CACHE-DROP  CUR @ EMIT-DIV-LIT-XT @ execute ;
: EMIT-MOD-LIT ( vreg n -- )
   over CACHE-DROP  CUR @ EMIT-MOD-LIT-XT @ execute ;

\ k when n is 2^k and k is 0 to 62, else -1.
: POW2-EXP ( n -- n )
   {: n:n :}
   63 0 ?do 1 i lshift n = if i unloop exit then loop
   -1 ;

: EMIT-NEG ( vreg -- ) dup CACHE-DROP  CUR @ EMIT-NEG-XT @ execute ;
: EMIT-NOT ( vreg -- ) dup CACHE-DROP  CUR @ EMIT-NOT-XT @ execute ;

\ EMIT-CMP and EMIT-CMP-LIT compare a with b (or with n) and set the condition
\ codes, writing no vreg; EMIT-IF-CC,, EMIT-WHILE-CC, and EMIT-UNTIL-CC, branch
\ on the codes. Between a compare and the word that reads its codes the front
\ end emits only moves: EMIT-PUSH, EMIT-PUSH-LIT, EMIT-SPILL, EMIT-SPILL-LIT,
\ EMIT-FILL, EMIT-POP, EMIT-LIT, EMIT-ADDR-DATA, EMIT-ADDR-CODE, EMIT-ADDR-QUOT,
\ EMIT-DSP+, EMIT-MOV and EMIT-EXCHANGE. Every emitter keeps those
\ condition-preserving.
\
\ EMIT-FLAG and EMIT-FLAG-LIT make d -1 when a c b (or a c n) holds, else 0: a
\ Habu flag. d may be a or b; every other vreg keeps its value. Both may
\ overwrite the condition codes, and neither reads them.
: EMIT-CMP ( vreg vreg -- ) CUR @ EMIT-CMP-XT @ execute ;
: EMIT-CMP-LIT ( vreg n -- ) CUR @ EMIT-CMP-LIT-XT @ execute ;
: EMIT-FLAG ( vreg vreg vreg cc -- ) {: d:vreg a:vreg b:vreg c:cc :}
   d CACHE-DROP  d a b c CUR @ EMIT-FLAG-XT @ execute ;
: EMIT-FLAG-LIT ( vreg vreg n cc -- ) {: d:vreg a:vreg v:n c:cc :}
   d CACHE-DROP  d a v c CUR @ EMIT-FLAG-LIT-XT @ execute ;

\ ---- memory ------------------------------------------------------------------
ARCHES TYPED-BUFFER EMIT-LOAD-XT [ vreg vreg width -- ]
ARCHES TYPED-BUFFER EMIT-STORE-XT [ vreg vreg width -- ]

\ EMIT-LOAD sets d to the cell or zero-extended byte at a. EMIT-STORE puts s's
\ cell or low byte at a and sets no vreg. Neither checks a, nor do kernel rows.
: EMIT-LOAD ( vreg vreg width -- ) {: d:vreg a:vreg w:width :}
   d CACHE-DROP  d a w CUR @ EMIT-LOAD-XT @ execute ;
: EMIT-STORE ( vreg vreg width -- ) CUR @ EMIT-STORE-XT @ execute ;

\ ---- calls -------------------------------------------------------------------
ARCHES TYPED-BUFFER EMIT-CALL-XT [ n n -- ]
ARCHES TYPED-BUFFER EMIT-TAIL-XT [ n n -- ]
ARCHES TYPED-BUFFER EMIT-CALL-SELF-XT [ -- ]

\ A call or tail branch to address to, for the token at body offset loc.
: EMIT-CALL ( n n -- ) CACHE-CLEAR  CUR @ EMIT-CALL-XT @ execute ;
: EMIT-TAIL ( n n -- ) CACHE-CLEAR  CUR @ EMIT-TAIL-XT @ execute ;

\ `recurse`: a call of byte 0, the definition, from any unit.
: EMIT-CALL-SELF ( -- ) CACHE-CLEAR  CUR @ EMIT-CALL-SELF-XT @ execute ;

\ ---- control -----------------------------------------------------------------
\ CONTROL. The front end rejects a structure out of order before calling one,
\ so these words trust the nesting. Branches, targets and open loop levels stay
\ behind the seam: an emitter that branches by label installs
\ src/habu/t0-label.f. The words a source keyword names end in a comma, since
\ a keyword cannot be a definition name.
ARCHES TYPED-BUFFER EMIT-IF,-XT [ vreg -- ]
ARCHES TYPED-BUFFER EMIT-ELSE,-XT [ -- ]
ARCHES TYPED-BUFFER EMIT-THEN,-XT [ -- ]
ARCHES TYPED-BUFFER EMIT-BEGIN,-XT [ -- ]
ARCHES TYPED-BUFFER EMIT-WHILE,-XT [ vreg -- ]
ARCHES TYPED-BUFFER EMIT-REPEAT,-XT [ -- ]
ARCHES TYPED-BUFFER EMIT-UNTIL,-XT [ vreg -- ]
ARCHES TYPED-BUFFER EMIT-IF-CC,-XT [ cc -- ]
ARCHES TYPED-BUFFER EMIT-WHILE-CC,-XT [ cc -- ]
ARCHES TYPED-BUFFER EMIT-UNTIL-CC,-XT [ cc -- ]
ARCHES TYPED-BUFFER EMIT-AGAIN,-XT [ -- ]
ARCHES TYPED-BUFFER EMIT-CASE,-XT [ -- ]
ARCHES TYPED-BUFFER EMIT-OF,-XT [ vreg vreg -- ]
ARCHES TYPED-BUFFER EMIT-OF-LIT-XT [ vreg n -- ]
ARCHES TYPED-BUFFER EMIT-ENDOF,-XT [ -- ]
ARCHES TYPED-BUFFER EMIT-ENDCASE,-XT [ -- ]

\ `if`: past the arm when the vreg is zero.
: EMIT-IF, ( vreg -- ) CUR @ EMIT-IF,-XT @ execute ;

\ `if` on condition c of the last compare: past the arm when c fails.
: EMIT-IF-CC, ( cc -- ) CUR @ EMIT-IF-CC,-XT @ execute ;

\ `else`: past the second arm, then the open `if` lands here.
: EMIT-ELSE, ( -- ) CUR @ EMIT-ELSE,-XT @ execute ;

\ `then`: the innermost open forward branch lands here.
: EMIT-THEN, ( -- ) CUR @ EMIT-THEN,-XT @ execute ;

: EMIT-BEGIN, ( -- ) CACHE-CLEAR  CUR @ EMIT-BEGIN,-XT @ execute ;

\ `while`: out of the loop when the vreg is zero; the exit stays open beneath
\ the head, as Forth orders orig and dest, so `begin ... while ... while ...
\ repeat then` resolves.
: EMIT-WHILE, ( vreg -- ) CUR @ EMIT-WHILE,-XT @ execute ;

\ `while` on condition c of the last compare: out of the loop when c fails.
: EMIT-WHILE-CC, ( cc -- ) CUR @ EMIT-WHILE-CC,-XT @ execute ;

\ `repeat`: back to the head, then the innermost `while` exit lands here.
: EMIT-REPEAT, ( -- ) CUR @ EMIT-REPEAT,-XT @ execute ;

\ `until`: back to the head when the vreg is zero.
: EMIT-UNTIL, ( vreg -- ) CUR @ EMIT-UNTIL,-XT @ execute ;

\ `until` on condition c of the last compare: back to the head when c fails.
: EMIT-UNTIL-CC, ( cc -- ) CUR @ EMIT-UNTIL-CC,-XT @ execute ;

: EMIT-AGAIN, ( -- ) CUR @ EMIT-AGAIN,-XT @ execute ;

\ `case` opens; EMIT-OF, and EMIT-OF-LIT branch past the arm when the selector
\ differs from the vreg or from n, which must answer EMIT-IMM? true; EMIT-ENDOF,
\ branches to the end and lands the open `of` here; EMIT-ENDCASE, lands every
\ `endof` exit here.
: EMIT-CASE, ( -- ) CUR @ EMIT-CASE,-XT @ execute ;
: EMIT-OF, ( vreg vreg -- ) CUR @ EMIT-OF,-XT @ execute ;
: EMIT-OF-LIT ( vreg n -- ) CUR @ EMIT-OF-LIT-XT @ execute ;
: EMIT-ENDOF, ( -- ) CUR @ EMIT-ENDOF,-XT @ execute ;
: EMIT-ENDCASE, ( -- ) CUR @ EMIT-ENDCASE,-XT @ execute ;

\ ---- the return stack --------------------------------------------------------
ARCHES TYPED-BUFFER EMIT-TOR-XT [ ^vreg n -- ]
ARCHES TYPED-BUFFER EMIT-RFROM-XT [ ^vreg n -- ]
ARCHES TYPED-BUFFER EMIT-RFETCH-XT [ ^vreg n n -- ]

: VREG-AT ( ^vreg n -- vreg ) cells + @ ;

: CACHE-DROPS ( ^vreg n -- ) {: p:^ n:n :}
   n 0 ?do p i VREG-AT CACHE-DROP loop ;

\ The return stack grows up from STACK-ABI:RETURN-BASE-CELL and is RSP-CELL
\ cells deep. Each operation moves the cells of the n vregs at p, at most
\ EMIT-VREGS, the first the deepest of n adjacent slots, and reads the depth
\ once: EMIT-TOR stores them in the slots above the top and EMIT-RFROM loads the
\ top n slots, each writing the depth once; EMIT-RFETCH loads the n slots whose
\ highest is k under the top and writes nothing.
: EMIT-TOR ( ^vreg n -- ) CUR @ EMIT-TOR-XT @ execute ;
: EMIT-RFROM ( ^vreg n -- )
   2dup CACHE-DROPS  CUR @ EMIT-RFROM-XT @ execute ;
: EMIT-RFETCH ( ^vreg n n -- ) {: p:^ n:n k:n :}
   p n CACHE-DROPS  p n k CUR @ EMIT-RFETCH-XT @ execute ;

\ ---- loops -------------------------------------------------------------------
ARCHES TYPED-BUFFER EMIT-LOOP-PUSH-XT [ vreg vreg -- ]
ARCHES TYPED-BUFFER EMIT-LOOP-POP-XT [ -- ]
ARCHES TYPED-BUFFER EMIT-LOOP-I-XT [ vreg -- ]
ARCHES TYPED-BUFFER EMIT-LOOP-J-XT [ vreg -- ]
ARCHES TYPED-BUFFER EMIT-LOOP-OPEN-XT [ vreg vreg bool -- ]
ARCHES TYPED-BUFFER EMIT-LEAVE,-XT [ -- ]
ARCHES TYPED-BUFFER EMIT-LOOP-NEXT-XT [ vreg -- ]
ARCHES TYPED-BUFFER EMIT-LOOP-STEP-XT [ vreg vreg -- ]
ARCHES TYPED-BUFFER EMIT-LOOP-CLOSE-XT [ -- ]

\ The DO/LOOP frames live on the DATA loop stack, where `j`, catch and
\ tier 0 find them: LOOPSP-CELL holds the address where the innermost 16-byte
\ frame ends, the stack's base while none is open. A frame holds the index, whose
\ home it is, then the limit, and the frame outside it ends where it starts.
\ `loop` steps while index < limit, signed; `+loop` steps until the index crosses
\ the boundary between limit - 1 and limit in the step's direction; `?do` enters
\ while start < limit, and EMIT-LOOP-STEP makes the entry of its level skip equal
\ bounds only. EMIT-LOOP-OPEN opens a
\ level; EMIT-LOOP-NEXT, EMIT-LOOP-STEP and EMIT-LEAVE, branch to the innermost
\ level's head or past its end; EMIT-LOOP-CLOSE lands its exits on the pop of
\ its frame. The loop words write no vreg but those passed in, so values in the
\ others live across every head, back edge and exit.

\ Push a frame of index s and limit l; pop the innermost frame.
: EMIT-LOOP-PUSH ( vreg vreg -- ) CUR @ EMIT-LOOP-PUSH-XT @ execute ;
: EMIT-LOOP-POP ( -- ) CUR @ EMIT-LOOP-POP-XT @ execute ;

\ The index of the innermost frame and of the one outside it into the vreg.
: EMIT-LOOP-I ( vreg -- )
   dup CACHE-DROP  CUR @ EMIT-LOOP-I-XT @ execute ;
: EMIT-LOOP-J ( vreg -- )
   dup CACHE-DROP  CUR @ EMIT-LOOP-J-XT @ execute ;

\ Open a level whose head is here. With entry true it is `?do`'s: past the
\ level unless start s < limit l.
: EMIT-LOOP-OPEN ( vreg vreg bool -- )
   CACHE-CLEAR  CUR @ EMIT-LOOP-OPEN-XT @ execute ;

\ `leave`: past the innermost level.
: EMIT-LEAVE, ( -- ) CUR @ EMIT-LEAVE,-XT @ execute ;

\ `loop`: step the index by one and branch back to the head while it is below
\ the limit. t is a free vreg it may clobber.
: EMIT-LOOP-NEXT ( vreg -- )
   dup CACHE-DROP  CUR @ EMIT-LOOP-NEXT-XT @ execute ;

\ `+loop`: step the index by s and branch back to the head unless it crossed
\ the boundary. s and t are clobbered; t is a free vreg.
: EMIT-LOOP-STEP ( vreg vreg -- )
   2dup CACHE-DROP CACHE-DROP  CUR @ EMIT-LOOP-STEP-XT @ execute ;

\ Land the innermost level's `leave` exits and entry here, close it and pop
\ its frame.
: EMIT-LOOP-CLOSE ( -- )
   CACHE-CLEAR  CUR @ EMIT-LOOP-CLOSE-XT @ execute ;

\ ---- the registry ------------------------------------------------------------
\ THE SEAM. Every cell has one slot per architecture: EMIT-PROVIDE fills one, EMIT-SELECT picks it
\ (E-T0-TARGET for none). An unset optional operation throws E-T0-UNFILLED (FILLED); every target fills the others.
: EMIT-PROVIDE ( CTARGET:arch [ n -- ] -- )
   swap ARCH>INDEX {: k:n :}
   k swap execute
   true k INSTALLED ! ;

: EMIT-PROVIDED? ( CTARGET:arch -- bool ) ARCH>INDEX INSTALLED @ ;

: EMIT-SELECT ( CTARGET:arch -- )
   {: a:CTARGET:arch :}
   a ARCH>INDEX {: k:n :}
   k INSTALLED @ 0= if E-T0-TARGET throw then
   k CUR !
   a CUR-ARCH ! ;

\ ---- sealing -----------------------------------------------------------------
\ A placed seal patches addresses as EMIT-PLACE's address plus the unit's
\ offset, since publishing only copies bytes; an unplaced one keeps the bytes.
\
\ THE CALLBACK STUB. While NCBSTUB is armed, the emission's first seal has the
\ emitter append the stub after the last unit and files it as the last
\ function, and later seals reuse it, so an unplaced seal's copy holds it.

: EMIT-PLACE ( n -- )
   {: at:n :}
   PHASE @ MATCH emission
      shut OF E-COMP-STATE throw ENDOF
      open OF ENDOF
      placed OF ENDOF
   ;MATCH
   at PLACED-AT !  construct emission placed PHASE ! ;

: NEMIT-KIND ( n -- n ) {: kind:n :}
   kind ADDR-QUOT-KIND = if NEMIT:ADDR-CODE exit then
   kind ;

: NEMIT-VALUE ( n n -- n )
   {: to:n kind:n :}
   kind ADDR-QUOT-KIND = if to EMIT-UNIT exit then
   to ;

\ The callback stub's offset, or -1 while NCBSTUB is not armed (THE CALLBACK STUB).
: EMIT-CB-STUB ( -- n )
   NCBSTUB:ARMED? 0= if -1 exit then
   STUB-AT @ 0 >= if STUB-AT @ exit then
   SLOT? if E-T0-SLOT throw then
   CUR @ EMIT-CB-STUB-XT @ FILLED execute {: stub:n :}
   stub STUB-AT !
   stub ;

\ No trailing return, so the record spans the whole emission.
: NEMIT-OPEN ( -- ) EMIT-BYTES 0 CUR-ARCH @ NEMIT:OPEN ;

: NEMIT-ROWS ( n -- ) {: stub:n :}
   N-UNITS @ 0 ?do i UNIT-AT @ NEMIT:FUNCTION+ loop
   stub 0 >= if stub NEMIT:FUNCTION+ then
   SLOT? if SLOT-AT @ NEMIT:PATCH-SLOT! then
   N-CALLS @ 0 ?do
      i CALL-AT @  i CALL-KIND-AT @  i CALL-TO @  0  i CALL-LOC @  NEMIT:CALL-SITE+
   loop
   N-ADDRS @ 0 ?do
      i EMIT-ADDR-ROW {: at:n kind:n to:n :}
      at  kind NEMIT-KIND  to kind NEMIT-VALUE  i ADDR-ORIGIN @  NEMIT:ADDR-SITE+
   loop
   NEMIT:SEAL ;

\ The stub goes in before EMIT-BYTES answers the emission it ends.
: SEAL-PLACED ( n -- )
   {: at:n :}
   EMIT-CB-STUB {: stub:n :}
   NEMIT-OPEN
   at EMIT-PATCH
   at NEMIT:PLACE
   stub 0 >= if at stub + NCBSTUB:ENTRY! then
   stub NEMIT-ROWS ;

: SEAL-OPEN ( -- )
   EMIT-CB-STUB {: stub:n :}
   NEMIT-OPEN
   stub NEMIT-ROWS ;

\ Shuts and clears PLACED-AT before anything can throw, so no image carries
\ this process's code address.
: EMIT-SEAL ( -- )
   PHASE @ {: phase:emission :}
   PLACED-AT @ {: at:n :}
   construct emission shut PHASE !  0 PLACED-AT !
   phase MATCH emission
      shut OF E-COMP-STATE throw ENDOF
      open OF SEAL-OPEN ENDOF
      placed OF at SEAL-PLACED ENDOF
   ;MATCH ;

: EMIT-SEAL-UNPLACED ( -- )
   PHASE @ MATCH emission
      shut OF E-COMP-STATE throw ENDOF
      open OF SEAL-OPEN ENDOF
      placed OF E-COMP-STATE throw ENDOF
   ;MATCH ;

;package

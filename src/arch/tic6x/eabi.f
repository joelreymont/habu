\ eabi.f - the C6000 EABI helpers TI's reused libraries need, emitted as C66x
\ instruction words from the constructors in src/arch/tic6x/asm.f: the integer
\ division family within SPRAB89's Table 8-9 register sets, memcpy/memset under
\ the standard convention, and bit-exact float division. Each helper is written
\ as a sequential program; the builder packs it into parallel execute packets
\ and supplies the load and branch delays itself.
require src/lib/error.f
require src/arch/tic6x/asm.f
require src/arch/tic6x/facts.f

package C6XEABI
public

E-C6XEABI-OPERAND constant E-OPERAND
E-C6XEABI-CAPACITY constant E-CAPACITY
10 constant HELPER-COUNT

private

1024 constant WORDS-MAX
16 constant FIXUP-MAX
256 constant PENDING-MAX
2048 constant CYCLE-MAX
BEGIN-STRUCTURE ROW-BYTES                   \ a pending instruction and its facts
   CELL +FIELD ROW.WORD
   CELL +FIELD ROW.UNIT
   CELL +FIELD ROW.CROSS
   CELL +FIELD ROW.READS
   CELL +FIELD ROW.WRITES
   CELL +FIELD ROW.LOADS
   CELL +FIELD ROW.MEMORY
   CELL +FIELD ROW.SIDE
END-STRUCTURE
BEGIN-STRUCTURE SLOT-BYTES                  \ one cycle's resources
   CELL +FIELD SLOT.UNITS                   \ units used
   CELL +FIELD SLOT.CROSS-1X                \ the B register read on 1X, or -1
   CELL +FIELD SLOT.CROSS-2X                \ the A register read on 2X, or -1
   CELL +FIELD SLOT.SIDES                   \ memory data paths used
END-STRUCTURE
BEGIN-STRUCTURE FIXUP-BYTES                 \ a forward branch awaiting its label
   CELL +FIELD FIXUP.AT                     \ word index, or -1 once resolved
   CELL +FIELD FIXUP.GUARD                  \ register code, or -1
   CELL +FIELD FIXUP.INVERTED
   CELL +FIELD FIXUP.SIDE                   \ the .S unit the branch took
END-STRUCTURE
create WORDS WORDS-MAX cells allot
create FIXUPS FIXUP-MAX FIXUP-BYTES * allot
create PERMITTED HELPER-COUNT cells allot   \ register bit masks: A0..A31 bits 0..31, B0..B31 bits 32..63
create PENDING PENDING-MAX ROW-BYTES * allot
create CYCLE-OF PENDING-MAX cells allot     \ the cycle each pending row issues in
create SLOTS CYCLE-MAX SLOT-BYTES * allot
variable LEN
variable FIXUP-COUNT
variable PENDING-COUNT
variable READY
variable LAST-CYCLE
variable DRAIN                              \ the cycle by which every pending load has landed
variable PLACED
variable BRANCH-AT
variable BRANCH-SIDE
variable IDLE-RUN
variable PACKET-START


: A ( n -- C6XASM:gpr ) C6XASM:A-REG ;
: B ( n -- C6XASM:gpr ) C6XASM:B-REG ;


\ ---- the program builder -------------------------------------------------------------
\ Instructions collect in a block until a label or a branch closes it. A list
\ scheduler then packs the block into execute packets: each instruction issues
\ in the earliest cycle in which its unit, cross path and memory data path are
\ free (SPRUGH7 3.8) and the sequential meaning holds: it reads every earlier
\ write, its writes land after earlier reads and strictly after earlier writes
\ of the same register, memory accesses keep their order unless both load, and
\ a value read through a cross path waits one more cycle so no stall is taken.
\ The branch closing a block issues as late as its guard allows but early
\ enough that its five delay slots hold the rest of the block and every load
\ has landed when the target runs, so no successor needs to know the block.

: ROW ( n -- ^n ) ROW-BYTES * PENDING + CELL-VIEW ;
: P-WORD ( n -- n ) ROW ROW.WORD @ ;
: P-UNIT ( n -- n ) ROW ROW.UNIT @ ;
: P-CROSS ( n -- n ) ROW ROW.CROSS @ ;
: P-READS ( n -- n ) ROW ROW.READS @ ;
: P-WRITES ( n -- n ) ROW ROW.WRITES @ ;
: P-LOADS ( n -- n ) ROW ROW.LOADS @ ;
: P-MEMORY ( n -- n ) ROW ROW.MEMORY @ ;
: P-SIDE ( n -- n ) ROW ROW.SIDE @ ;
: P-CYCLE ( n -- n ) cells CYCLE-OF + @ ;
: SLOT ( n -- ^n ) SLOT-BYTES * SLOTS + CELL-VIEW ;
: FIXUP ( n -- ^n ) FIXUP-BYTES * FIXUPS + CELL-VIEW ;

: FILL-ROW ( n n -- ) {: w:n idx:n :}
   w C6XFACTS:CLASSIFY C6XFACTS-FACTS:UNMAKE
   {: unit:n cross:n reads:n writes:n loads:n memory:n side:n branch:bool idle:n :}
   idx ROW {: row:^ :}
   w row ROW.WORD ! unit row ROW.UNIT ! cross row ROW.CROSS ! reads row ROW.READS !
   writes row ROW.WRITES ! loads row ROW.LOADS ! memory row ROW.MEMORY ! side row ROW.SIDE ! ;

: EMIT ( C6XASM:instruction -- )
   PENDING-COUNT @ PENDING-MAX >= if E-CAPACITY throw then
   C6XASM:INSTRUCTION>N {: w:n :}
   w C6XFACTS:CLASSIFY C6XFACTS-FACTS:UNMAKE
   {: unit:n cross:n reads:n writes:n loads:n memory:n side:n branch:bool idle:n :}
   idle 0 <> branch or if E-OPERAND throw then              \ idles and branches belong to the scheduler
   w PENDING-COUNT @ FILL-ROW 1 PENDING-COUNT +! ;

: RAISE ( n -- ) READY @ max READY ! ;

\ Raises READY so row idx keeps its sequential meaning after the placed row j.
: AFTER ( n n -- ) {: idx:n j:n :}
   j P-CYCLE {: c:n :}
   idx P-READS {: reads:n :} idx P-WRITES {: writes:n :} idx P-LOADS {: loads:n :}
   idx P-CROSS 0 >= if idx P-CROSS C6XFACTS:MASK-BIT else 0 then {: crossed:n :}
   j P-WRITES reads and 0 <> if c 1+ RAISE then
   j P-WRITES crossed and 0 <> if c 2 + RAISE then
   j P-LOADS reads and 0 <> if c C6XFACTS:LOAD-LATENCY + RAISE then
   j P-READS writes and 0 <> if c RAISE then
   j P-READS loads and 0 <> if c 1+ C6XFACTS:LOAD-LATENCY - RAISE then
   j P-WRITES writes and 0 <> if c 1+ RAISE then
   j P-LOADS writes and 0 <> if c C6XFACTS:LOAD-LATENCY + RAISE then
   j P-WRITES loads and 0 <> if c 2 + C6XFACTS:LOAD-LATENCY - RAISE then
   j P-LOADS loads and 0 <> if c 1+ RAISE then
   j P-MEMORY 0 <> idx P-MEMORY 0 <> and j P-MEMORY 1 = idx P-MEMORY 1 = and 0= and if c 1+ RAISE then ;

\ Whether cycle has room for row idx: its unit, its cross path (one register
\ per path per cycle) and, for a memory access, its data path.
: FREE? ( n n -- bool ) {: idx:n cycle:n :}
   cycle SLOT {: slot:^ :}
   slot SLOT.UNITS @ idx P-UNIT C6XFACTS:MASK-BIT and 0 <> if FALSE exit then
   idx P-CROSS 0 >= if
      idx P-CROSS 32 >= if slot SLOT.CROSS-1X else slot SLOT.CROSS-2X then @ {: held:n :}
      held 0 >= held idx P-CROSS <> and if FALSE exit then
   then
   idx P-MEMORY 0 <> if slot SLOT.SIDES @ idx P-SIDE C6XFACTS:MASK-BIT and 0 <> if FALSE exit then then
   TRUE ;

: CLAIM ( n n -- ) {: idx:n cycle:n :}
   cycle SLOT {: slot:^ :}
   slot SLOT.UNITS @ idx P-UNIT C6XFACTS:MASK-BIT or slot SLOT.UNITS !
   idx P-CROSS 0 >= if idx P-CROSS idx P-CROSS 32 >= if slot SLOT.CROSS-1X else slot SLOT.CROSS-2X then ! then
   idx P-MEMORY 0 <> if slot SLOT.SIDES @ idx P-SIDE C6XFACTS:MASK-BIT or slot SLOT.SIDES ! then
   cycle idx cells CYCLE-OF + ! ;

\ Places row idx in the earliest legal cycle after rows 0 to idx-1.
: PLACE ( n -- ) {: idx:n :}
   0 READY !
   idx 0 ?do idx i AFTER loop
   begin idx READY @ FREE? 0= while
      1 READY +! READY @ CYCLE-MAX 8 - >= if E-CAPACITY throw then
   repeat
   idx READY @ CLAIM ;

: CLEAR-SLOTS ( -- )
   CYCLE-MAX 0 ?do
      i SLOT {: slot:^ :}
      0 slot SLOT.UNITS ! -1 slot SLOT.CROSS-1X ! -1 slot SLOT.CROSS-2X ! 0 slot SLOT.SIDES !
   loop ;

: SCHEDULE-BLOCK ( -- )
   CLEAR-SLOTS
   -1 LAST-CYCLE ! 0 DRAIN !
   PENDING-COUNT @ 0 ?do
      i PLACE
      i P-CYCLE LAST-CYCLE @ max LAST-CYCLE !
      i P-LOADS 0 <> if i P-CYCLE C6XFACTS:LOAD-LATENCY + DRAIN @ max DRAIN ! then
   loop ;

: BRANCH-UNIT! ( n -- ) PENDING-COUNT @ ROW ROW.UNIT ! ;

\ Places the branch row, index PENDING-COUNT, after the block; a relative
\ branch takes whichever .S unit is free first.
: PLACE-BRANCH ( n -- ) {: flexible:n :}
   PENDING-COUNT @ {: b:n :}
   0 READY ! 0 PLACED !
   b 0 ?do b i AFTER loop
   LAST-CYCLE @ 5 - RAISE DRAIN @ 6 - RAISE
   begin PLACED @ 0= while
      READY @ CYCLE-MAX 8 - >= if E-CAPACITY throw then
      flexible 0 <> if C6XFACTS:UNIT-S 2 * BRANCH-UNIT! then
      b READY @ FREE? if 1 PLACED ! else
         flexible 0 <> if C6XFACTS:UNIT-S 2 * 1+ BRANCH-UNIT! b READY @ FREE? if 1 PLACED ! then then
      then
      PLACED @ 0= if 1 READY +! then
   repeat
   b READY @ CLAIM
   b P-UNIT 1 and BRANCH-SIDE ! ;

: EMIT-WORD ( n -- )
   LEN @ WORDS-MAX >= if E-CAPACITY throw then
   WORDS LEN @ cells + ! 1 LEN +! ;

: EMIT-IDLE ( -- )
   begin IDLE-RUN @ 0 > while
      IDLE-RUN @ 9 min {: n:n :}
      n C6XASM:ENC-NOP C6XASM:INSTRUCTION>N EMIT-WORD
      IDLE-RUN @ n - IDLE-RUN !
   repeat ;

\ Lays out one cycle: its rows chained by the p bit, or one more idle cycle.
: LAYOUT-CYCLE ( n n -- ) {: cycle:n count:n :}
   -1 PACKET-START !
   count 0 ?do
      i P-CYCLE cycle = if
         PACKET-START @ 0 < if EMIT-IDLE LEN @ PACKET-START !
         else WORDS LEN @ 1- cells + dup @ 1 or swap ! then
         i PENDING-COUNT @ = if LEN @ BRANCH-AT ! then
         i P-WORD EMIT-WORD
      then
   loop
   PACKET-START @ 0 < if 1 IDLE-RUN +! then ;

\ Lays out total cycles of count rows; the branch row, when present, is the last.
: LAYOUT ( n n -- ) {: total:n count:n :}
   -1 BRANCH-AT ! 0 IDLE-RUN !
   total 0 ?do i count LAYOUT-CYCLE loop
   EMIT-IDLE 0 PENDING-COUNT ! ;

: FLUSH ( -- )
   PENDING-COUNT @ 0= if exit then
   SCHEDULE-BLOCK
   LAST-CYCLE @ 1+ DRAIN @ max PENDING-COUNT @ LAYOUT ;

\ Closes the block with branch word w, whose displacement may still be zero;
\ returns the word index the branch landed at, for patching.
: CLOSE ( n n -- n ) {: w:n flexible:n :}
   PENDING-COUNT @ PENDING-MAX >= if E-CAPACITY throw then
   w PENDING-COUNT @ FILL-ROW
   SCHEDULE-BLOCK flexible PLACE-BRANCH
   READY @ 6 + PENDING-COUNT @ 1+ LAYOUT
   BRANCH-AT @ ;

\ A label: the block so far is laid out, and the next word starts a packet.
: HERE ( -- n ) FLUSH LEN @ ;

\ Branch displacements count from the fetch packet holding the branch; every
\ helper therefore starts on a 32-byte boundary.
: DISPLACEMENT ( n n -- n ) {: from:n target:n :}
   target 4 * from 4 * $FFFFFFE0 and - ;

\ Guards for branches: a register code or -1, and whether the zero case branches.
: NEVER ( -- n n ) -1 0 ;
: WHEN ( C6XASM:gpr -- n n ) C6XASM:GPR>N 0 ;
: UNLESS ( C6XASM:gpr -- n n ) C6XASM:GPR>N 1 ;

: GUARDED ( C6XASM:instruction n n -- C6XASM:instruction ) {: opcode:C6XASM:instruction guard:n inverted:n :}
   guard 0 < if opcode exit then
   inverted 0 <> if opcode guard C6XASM:>GPR C6XASM:WHEN-ZERO else opcode guard C6XASM:>GPR C6XASM:WHEN-NONZERO then ;

: REL ( n n n n -- C6XASM:instruction ) {: side:n delta:n guard:n inverted:n :}
   side C6XASM:>SIDE delta C6XASM:>BRANCH-OFFSET C6XASM:ENC-B-REL guard inverted GUARDED ;

\ Replaces the word at index at, keeping the p bit the layout gave it.
: PATCH ( n C6XASM:instruction -- ) {: at:n opcode:C6XASM:instruction :}
   WORDS at cells + @ 1 and opcode C6XASM:INSTRUCTION>N or WORDS at cells + ! ;

: BACK ( n n n -- ) {: target:n guard:n inverted:n :}
   0 0 guard inverted REL C6XASM:INSTRUCTION>N 1 CLOSE {: at:n :}
   at BRANCH-SIDE @ at target DISPLACEMENT guard inverted REL PATCH ;

\ A branch forward to a label not yet placed; RESOLVE aims it at HERE.
: FORWARD ( n n -- n ) {: guard:n inverted:n :}
   FIXUP-COUNT @ FIXUP-MAX >= if E-CAPACITY throw then
   0 0 guard inverted REL C6XASM:INSTRUCTION>N 1 CLOSE {: at:n :}
   FIXUP-COUNT @ FIXUP {: row:^ :}
   at row FIXUP.AT ! guard row FIXUP.GUARD ! inverted row FIXUP.INVERTED ! BRANCH-SIDE @ row FIXUP.SIDE !
   FIXUP-COUNT @ 1 FIXUP-COUNT +! ;

: RESOLVE ( n -- ) {: ref:n :}
   ref FIXUP {: row:^ :}
   row FIXUP.AT @ 0 < if E-OPERAND throw then               \ resolved twice
   HERE {: target:n :}
   row FIXUP.AT @ {: at:n :}
   at row FIXUP.SIDE @ at target DISPLACEMENT row FIXUP.GUARD @ row FIXUP.INVERTED @ REL PATCH
   -1 row FIXUP.AT ! ;

: RETURN ( -- ) 3 B C6XASM:ENC-B-REG C6XASM:INSTRUCTION>N 0 CLOSE drop ;

: RESOLVED ( -- )
   FIXUP-COUNT @ 0 ?do i FIXUP FIXUP.AT @ 0 >= if E-OPERAND throw then loop ;


\ ---- unsigned division ------------------------------------------------------------------

\ Divides n by d, both unsigned: the quotient lands in q, the remainder stays
\ in n. Exact and loop-free: a 32-step SUBC chain predicated on the step count
\ c serves dividends and divisors below 2^31, a divisor with its top bit set
\ needs one compare, and a dividend with its top bit set is halved first and
\ rejoined afterwards, so the chain's 32-bit shift never overflows. t (n's
\ bank) and u (d's bank) are scratch, g (n's bank) and h are guards, c is a
\ guard on d's bank, and B30 keeps the divisor. A zero divisor gives a
\ quotient of $FFFFFFFF and leaves n.
: DIVIDE-U ( C6XASM:gpr C6XASM:gpr C6XASM:gpr C6XASM:gpr C6XASM:gpr C6XASM:gpr C6XASM:gpr C6XASM:gpr -- ) {: n:C6XASM:gpr d:C6XASM:gpr q:C6XASM:gpr t:C6XASM:gpr u:C6XASM:gpr g:C6XASM:gpr h:C6XASM:gpr c:C6XASM:gpr :}
   q 0 C6XASM:ENC-MVK EMIT
   t 0 C6XASM:ENC-MVK EMIT
   g t d C6XASM:ENC-CMPEQ-L EMIT                           \ zero divisor
   q -1 C6XASM:ENC-MVK g C6XASM:WHEN-NONZERO EMIT
   g WHEN FORWARD {: zero-ref:n :}
   c d 0 C6XASM:ALWAYS C6XASM:ENC-CMPGT-I5 EMIT                   \ top bit set: the quotient is 0 or 1
   c UNLESS FORWARD {: normal:n :}
   g n d C6XASM:ALWAYS C6XASM:ENC-CMPLTU-L EMIT
   q 1 C6XASM:ENC-MVK EMIT
   q 0 C6XASM:ENC-MVK g C6XASM:WHEN-NONZERO EMIT
   n n d C6XASM:ENC-SUB-L g C6XASM:WHEN-ZERO EMIT
   NEVER FORWARD {: done:n :}
   normal RESOLVE
   h n 0 C6XASM:ALWAYS C6XASM:ENC-CMPGT-I5 EMIT                   \ dividend top bit set: divide half of it
   t n 31 31 C6XASM:ALWAYS C6XASM:ENC-EXTU-S EMIT                 \ the bit split off
   n n 1 h C6XASM:IF-NONZERO C6XASM:ENC-SHRU-U5 EMIT
   30 B d C6XASM:ENC-MV-L EMIT
   u d C6XASM:ALWAYS C6XASM:ENC-NORM-L EMIT                       \ leading zeros less one, both below 2^31
   c n C6XASM:ALWAYS C6XASM:ENC-NORM-L EMIT
   u u c C6XASM:ENC-SUB-L EMIT                             \ the shift aligning the divisor
   c u 0 C6XASM:ALWAYS C6XASM:ENC-CMPGT-I5 EMIT                   \ negative: the dividend is smaller
   c WHEN FORWARD {: small:n :}
   d d u C6XASM:ALWAYS C6XASM:ENC-SHL-S EMIT
   c u 1 C6XASM:ENC-ADD-I5 EMIT                            \ steps
   32 0 ?do
      n n d c C6XASM:IF-NONZERO C6XASM:ENC-SUBC-L EMIT
      c c -1 C6XASM:ENC-ADD-I5 c C6XASM:WHEN-NONZERO EMIT
   loop
   g 31 C6XASM:ENC-MVK EMIT
   g g u C6XASM:ENC-SUB-L EMIT                             \ 31 - shift
   q n g C6XASM:ALWAYS C6XASM:ENC-SHL-S EMIT                      \ the quotient bits
   q q g C6XASM:ALWAYS C6XASM:ENC-SHRU-S EMIT
   g u 1 C6XASM:ENC-ADD-I5 EMIT
   n n g C6XASM:ALWAYS C6XASM:ENC-SHRU-S EMIT                     \ the remainder
   small RESOLVE
   n n 1 h C6XASM:IF-NONZERO C6XASM:ENC-SHL-U5 EMIT               \ rejoin the split bit
   n n t C6XASM:ENC-ADD-L h C6XASM:WHEN-NONZERO EMIT
   q q 1 h C6XASM:IF-NONZERO C6XASM:ENC-SHL-U5 EMIT
   g n 30 B C6XASM:ALWAYS C6XASM:ENC-CMPLTU-L EMIT                \ does one more divisor fit?
   n n 30 B C6XASM:ENC-SUB-L g C6XASM:WHEN-ZERO EMIT
   q q 1 C6XASM:ENC-ADD-I5 g C6XASM:WHEN-ZERO EMIT
   done RESOLVE zero-ref RESOLVE ;


\ Negates r in place when guard is nonzero, through a zero in scratch.
: NEGATE-WHEN ( C6XASM:gpr C6XASM:gpr C6XASM:gpr -- ) {: r:C6XASM:gpr scratch:C6XASM:gpr guard:C6XASM:gpr :}
   scratch 0 C6XASM:ENC-MVK EMIT
   r scratch r C6XASM:ENC-SUB-L guard C6XASM:WHEN-NONZERO EMIT ;


\ ---- the helpers ---------------------------------------------------------------------

: DIVU ( -- )
   4 A 4 B 6 A 1 A 1 B 0 A 2 A 0 B DIVIDE-U
   4 A 6 A C6XASM:ENC-MV-L EMIT RETURN ;

: REMU ( -- )
   4 A 4 B 5 A 7 A 1 B 1 A 2 B 0 B DIVIDE-U
   RETURN ;

: DIVREMU ( -- )
   4 A 4 B 6 A 1 A 1 B 0 A 2 A 0 B DIVIDE-U
   5 A 4 A C6XASM:ENC-MV-L EMIT 4 A 6 A C6XASM:ENC-MV-L EMIT RETURN ;

\ |r| by conditional negation through a zero in scratch: the C66x ABS
\ saturates INT32_MIN to INT32_MAX (SPRUGH7), while the wrap contract needs
\ the exact 32-bit magnitude $80000000.
: MAGNITUDE ( C6XASM:gpr C6XASM:gpr C6XASM:gpr -- ) {: r:C6XASM:gpr zero:C6XASM:gpr guard:C6XASM:gpr :}
   zero 0 C6XASM:ENC-MVK EMIT
   guard r 0 C6XASM:ALWAYS C6XASM:ENC-CMPGT-I5 EMIT
   r zero r C6XASM:ENC-SUB-L guard C6XASM:WHEN-NONZERO EMIT ;

: MAGNITUDES ( -- )
   4 A 1 A 2 A MAGNITUDE 4 B 1 B 0 B MAGNITUDE ;

: DIVI ( -- )
   5 B 4 B 4 A C6XASM:ENC-XOR-L EMIT                       \ the quotient's sign
   MAGNITUDES
   4 A 4 B 6 A 1 A 1 B 0 A 2 A 0 B DIVIDE-U
   2 B 5 B 0 C6XASM:ALWAYS C6XASM:ENC-CMPGT-I5 EMIT
   6 A 1 A 2 B NEGATE-WHEN
   4 A 6 A C6XASM:ENC-MV-L EMIT RETURN ;

: REMI ( -- )
   2 B 4 A 0 C6XASM:ALWAYS C6XASM:ENC-CMPGT-I5 EMIT               \ the remainder takes the dividend's sign
   MAGNITUDES
   4 A 4 B 6 A 5 A 1 B 1 A 2 A 0 B DIVIDE-U
   4 A 1 A 2 B NEGATE-WHEN
   RETURN ;

: DIVREMI ( -- )
   31 B 4 B 4 A C6XASM:ENC-XOR-L EMIT
   2 B 4 A 0 C6XASM:ALWAYS C6XASM:ENC-CMPGT-I5 EMIT
   MAGNITUDES
   4 A 4 B 6 A 5 A 1 B 1 A 2 A 0 B DIVIDE-U
   4 A 1 A 2 B NEGATE-WHEN
   2 B 31 B 0 C6XASM:ALWAYS C6XASM:ENC-CMPGT-I5 EMIT
   6 A 1 A 2 B NEGATE-WHEN
   5 A 4 A C6XASM:ENC-MV-L EMIT 4 A 6 A C6XASM:ENC-MV-L EMIT RETURN ;

\ A2 bytes from *B4++ to *A5++ through A7; A2 may be zero.
: COPY-BYTES ( -- )
   2 A UNLESS FORWARD {: done:n :}
   HERE {: loop:n :}
   7 A 4 B 1 C6XASM:>BYTE-OFFSET C6XASM:ALWAYS C6XASM:ENC-LDB++ EMIT
   7 A 5 A 1 C6XASM:>BYTE-OFFSET C6XASM:ALWAYS C6XASM:ENC-STB++ EMIT
   2 A 2 A -1 C6XASM:ENC-ADD-I5 EMIT
   loop 2 A WHEN BACK
   done RESOLVE ;

\ A2 bytes of B5's low byte to *A5++; A2 may be zero.
: FILL-BYTES ( -- )
   2 A UNLESS FORWARD {: done:n :}
   HERE {: loop:n :}
   5 B 5 A 1 C6XASM:>BYTE-OFFSET C6XASM:ALWAYS C6XASM:ENC-STB++ EMIT
   2 A 2 A -1 C6XASM:ENC-ADD-I5 EMIT
   loop 2 A WHEN BACK
   done RESOLVE ;

\ The bytes that bring the destination A5 to an 8-byte boundary, bounded by
\ the count A1, into A2, with A1 reduced accordingly.
: HEAD-BYTES ( -- )
   8 A 5 A 29 29 C6XASM:ALWAYS C6XASM:ENC-EXTU-S EMIT               \ A8 = dst & 7
   2 A 8 C6XASM:ENC-MVK EMIT
   8 A 2 A 8 A C6XASM:ENC-SUB-L EMIT
   8 A 8 A 29 29 C6XASM:ALWAYS C6XASM:ENC-EXTU-S EMIT               \ (8 - (dst & 7)) & 7
   0 A 1 A 8 A C6XASM:ALWAYS C6XASM:ENC-CMPLTU-L EMIT
   8 A 1 A C6XASM:ENC-MV-L 0 A C6XASM:WHEN-NONZERO EMIT             \ no more than the count
   2 A 8 A C6XASM:ENC-MV-L EMIT
   1 A 1 A 8 A C6XASM:ENC-SUB-L EMIT ;

\ The doubleword count of the remaining A1 bytes into A2, A1 keeping the tail;
\ zero when guard (B0) says the fast path does not apply.
: DOUBLEWORDS ( -- )
   8 A 1 A 3 C6XASM:ALWAYS C6XASM:ENC-SHRU-U5 EMIT
   8 A 0 C6XASM:ENC-MVK 0 B C6XASM:WHEN-NONZERO EMIT
   2 A 8 A C6XASM:ENC-MV-L EMIT
   8 A 8 A 3 C6XASM:ALWAYS C6XASM:ENC-SHL-U5 EMIT
   1 A 1 A 8 A C6XASM:ENC-SUB-L EMIT ;

\ memcpy(dst A4, src B4, n A6) returns dst. Bytes align the destination,
\ doublewords follow when the source shares that alignment, bytes finish.
: MEMCPY ( -- )
   5 A 4 A C6XASM:ENC-MV-L EMIT 1 A 6 A C6XASM:ENC-MV-L EMIT
   HEAD-BYTES COPY-BYTES
   0 B 4 B 29 29 C6XASM:ALWAYS C6XASM:ENC-EXTU-S EMIT               \ B0 = the source is still misaligned
   DOUBLEWORDS
   2 A UNLESS FORWARD {: tail:n :}
   HERE {: loop:n :}
   16 A 4 B 8 C6XASM:>BYTE-OFFSET C6XASM:ALWAYS C6XASM:ENC-LDDW++ EMIT
   16 A 5 A 8 C6XASM:>BYTE-OFFSET C6XASM:ALWAYS C6XASM:ENC-STDW++ EMIT
   2 A 2 A -1 C6XASM:ENC-ADD-I5 EMIT
   loop 2 A WHEN BACK
   tail RESOLVE
   2 A 1 A C6XASM:ENC-MV-L EMIT
   COPY-BYTES RETURN ;

\ memset(dst A4, c B4, n A6) returns dst: bytes to an 8-byte boundary, then
\ doublewords of the replicated byte, then the tail.
: MEMSET ( -- )
   5 A 4 A C6XASM:ENC-MV-L EMIT 1 A 6 A C6XASM:ENC-MV-L EMIT
   5 B 4 B 24 24 C6XASM:ALWAYS C6XASM:ENC-EXTU-S EMIT               \ B5 = the byte
   6 B 5 B 8 C6XASM:ALWAYS C6XASM:ENC-SHL-U5 EMIT
   5 B 5 B 6 B C6XASM:ENC-OR-L EMIT
   6 B 5 B 16 C6XASM:ALWAYS C6XASM:ENC-SHL-U5 EMIT
   5 B 5 B 6 B C6XASM:ENC-OR-L EMIT                          \ replicated in a word
   6 B 5 B C6XASM:ENC-MV-L EMIT 7 B 5 B C6XASM:ENC-MV-L EMIT        \ and in the pair B7:B6
   HEAD-BYTES FILL-BYTES
   0 B 0 C6XASM:ENC-MVK EMIT
   DOUBLEWORDS
   2 A UNLESS FORWARD {: tail:n :}
   HERE {: loop:n :}
   6 B 5 A 8 C6XASM:>BYTE-OFFSET C6XASM:ALWAYS C6XASM:ENC-STDW++ EMIT
   2 A 2 A -1 C6XASM:ENC-ADD-I5 EMIT
   loop 2 A WHEN BACK
   tail RESOLVE
   2 A 1 A C6XASM:ENC-MV-L EMIT
   FILL-BYTES RETURN ;


\ ---- float32 division: bit-exact IEEE 754, round to nearest even -------------------

\ divf(x A4, y B4) under the standard convention. Significands divide by a
\ restoring loop into 26 bits (24 plus guard and round) with the remainder as
\ sticky; denormal results shift right first; NaNs return the first NaN operand
\ quieted, invalid operations the default NaN $7FC00000.
: DIVF ( -- )
   5 A 4 A 1 24 C6XASM:ALWAYS C6XASM:ENC-EXTU-S EMIT                  \ A5 = exponent of x
   5 B 4 B 1 24 C6XASM:ALWAYS C6XASM:ENC-EXTU-S EMIT                  \ B5 = exponent of y
   6 A 4 A 9 9 C6XASM:ALWAYS C6XASM:ENC-EXTU-S EMIT                   \ A6 = fraction of x
   6 B 4 B 9 9 C6XASM:ALWAYS C6XASM:ENC-EXTU-S EMIT                   \ B6 = fraction of y
   7 A 4 A 4 B C6XASM:ENC-XOR-L EMIT                           \ A7 = the result's sign bit
   7 A 7 A 0 30 C6XASM:ALWAYS C6XASM:ENC-CLR-S EMIT
   17 A 255 C6XASM:ENC-MVK EMIT
   0 A 17 A 5 A C6XASM:ENC-CMPEQ-L EMIT                        \ A0 = x has the top exponent
   1 A 17 A 5 B C6XASM:ENC-CMPEQ-L EMIT                        \ A1 = y has the top exponent
   2 A 6 A 0 C6XASM:ALWAYS C6XASM:ENC-CMPLTU-U4 EMIT
   2 A 2 A 0 A C6XASM:ENC-AND-L EMIT                           \ x is NaN
   2 A WHEN FORWARD {: x-nan:n :}
   2 B 6 B 0 C6XASM:ALWAYS C6XASM:ENC-CMPLTU-U4 EMIT
   2 B 2 B 1 A C6XASM:ENC-AND-L EMIT                           \ y is NaN
   2 B WHEN FORWARD {: y-nan:n :}
   2 A 0 A 1 A C6XASM:ENC-AND-L EMIT                           \ both infinite
   2 A WHEN FORWARD {: invalid:n :}
   0 A WHEN FORWARD {: infinite:n :}                    \ x infinite
   1 A WHEN FORWARD {: zero:n :}                        \ y infinite
   0 B 5 B 1 C6XASM:ALWAYS C6XASM:ENC-CMPGTU-U4 EMIT                  \ B0 = y has a zero exponent
   1 B 6 B 1 C6XASM:ALWAYS C6XASM:ENC-CMPGTU-U4 EMIT
   2 B 0 B 1 B C6XASM:ENC-AND-L EMIT                           \ y is zero
   0 A 5 A 1 C6XASM:ALWAYS C6XASM:ENC-CMPGTU-U4 EMIT                  \ A0 = x has a zero exponent
   1 A 6 A 1 C6XASM:ALWAYS C6XASM:ENC-CMPGTU-U4 EMIT
   2 A 0 A 1 A C6XASM:ENC-AND-L EMIT                           \ x is zero
   1 B 2 B 2 A C6XASM:ENC-AND-L EMIT
   1 B WHEN FORWARD {: invalid2:n :}                    \ 0 / 0
   2 B WHEN FORWARD {: infinite2:n :}                   \ x / 0
   2 A WHEN FORWARD {: zero2:n :}                       \ 0 / y
   17 A 6 A C6XASM:ALWAYS C6XASM:ENC-NORM-L EMIT                      \ normalise a denormal x
   17 A 17 A -7 C6XASM:ENC-ADD-I5 EMIT
   6 A 6 A 17 A 0 A C6XASM:IF-NONZERO C6XASM:ENC-SHL-S EMIT
   5 A 1 C6XASM:ENC-MVK 0 A C6XASM:WHEN-NONZERO EMIT
   5 A 5 A 17 A C6XASM:ENC-SUB-L 0 A C6XASM:WHEN-NONZERO EMIT
   6 A 6 A 23 23 0 A C6XASM:IF-ZERO C6XASM:ENC-SET-S EMIT              \ or restore the hidden bit
   9 B 6 B C6XASM:ALWAYS C6XASM:ENC-NORM-L EMIT                       \ the same for y
   9 B 9 B -7 C6XASM:ENC-ADD-I5 EMIT
   6 B 6 B 9 B 0 B C6XASM:IF-NONZERO C6XASM:ENC-SHL-S EMIT
   5 B 1 C6XASM:ENC-MVK 0 B C6XASM:WHEN-NONZERO EMIT
   5 B 5 B 9 B C6XASM:ENC-SUB-L 0 B C6XASM:WHEN-NONZERO EMIT
   6 B 6 B 23 23 0 B C6XASM:IF-ZERO C6XASM:ENC-SET-S EMIT
   8 A 5 A 5 B C6XASM:ENC-SUB-L EMIT                           \ A8 = ex - ey + 127
   17 A 127 C6XASM:ENC-MVK EMIT
   8 A 8 A 17 A C6XASM:ENC-ADD-L EMIT
   0 A 6 A 6 B C6XASM:ALWAYS C6XASM:ENC-CMPLTU-L EMIT                 \ A0 = mx < my: a quotient below one
   9 A 1 C6XASM:ENC-MVK EMIT
   16 A 6 A 6 B C6XASM:ENC-SUB-L EMIT
   1 B 25 C6XASM:ENC-MVK EMIT
   9 A 0 C6XASM:ENC-MVK 0 A C6XASM:WHEN-NONZERO EMIT
   16 A 6 A C6XASM:ENC-MV-L 0 A C6XASM:WHEN-NONZERO EMIT
   8 A 8 A -1 C6XASM:ENC-ADD-I5 0 A C6XASM:WHEN-NONZERO EMIT
   1 B 26 C6XASM:ENC-MVK 0 A C6XASM:WHEN-NONZERO EMIT
   HERE {: loop:n :}
   16 A 16 A 1 C6XASM:ALWAYS C6XASM:ENC-SHL-U5 EMIT
   9 A 9 A 1 C6XASM:ALWAYS C6XASM:ENC-SHL-U5 EMIT
   1 A 16 A 6 B C6XASM:ALWAYS C6XASM:ENC-CMPLTU-L EMIT
   16 A 16 A 6 B C6XASM:ENC-SUB-L 1 A C6XASM:WHEN-ZERO EMIT
   9 A 9 A 1 C6XASM:ENC-ADD-I5 1 A C6XASM:WHEN-ZERO EMIT
   1 B 1 B -1 C6XASM:ENC-ADD-I5 EMIT
   loop 1 B WHEN BACK
   8 B 16 A 0 C6XASM:ALWAYS C6XASM:ENC-CMPLTU-U4 EMIT                 \ B8 = sticky
   1 A 8 A 1 C6XASM:ALWAYS C6XASM:ENC-CMPGT-I5 EMIT                   \ A1 = the result is denormal
   17 A 1 C6XASM:ENC-MVK EMIT
   17 A 17 A 8 A C6XASM:ENC-SUB-L EMIT                         \ A17 = extra right shift
   17 A 0 C6XASM:ENC-MVK 1 A C6XASM:WHEN-ZERO EMIT
   18 A 27 C6XASM:ENC-MVK EMIT
   2 A 17 A 18 A C6XASM:ALWAYS C6XASM:ENC-CMPGT-L EMIT
   17 A 27 C6XASM:ENC-MVK 2 A C6XASM:WHEN-NONZERO EMIT
   18 A 1 C6XASM:ENC-MVK EMIT
   18 A 18 A 17 A C6XASM:ALWAYS C6XASM:ENC-SHL-S EMIT
   18 A 18 A -1 C6XASM:ENC-ADD-I5 EMIT
   18 A 18 A 9 A C6XASM:ENC-AND-L EMIT                         \ bits about to be shifted out
   2 A 18 A 0 C6XASM:ALWAYS C6XASM:ENC-CMPLTU-U4 EMIT
   8 B 8 B 2 A C6XASM:ENC-OR-L EMIT
   9 A 9 A 17 A C6XASM:ALWAYS C6XASM:ENC-SHRU-S EMIT
   8 A 0 C6XASM:ENC-MVK 1 A C6XASM:WHEN-NONZERO EMIT
   18 A 9 A 30 31 C6XASM:ALWAYS C6XASM:ENC-EXTU-S EMIT                \ guard
   19 A 9 A 31 31 C6XASM:ALWAYS C6XASM:ENC-EXTU-S EMIT                \ round
   9 A 9 A 2 C6XASM:ALWAYS C6XASM:ENC-SHRU-U5 EMIT
   20 A 9 A 31 31 C6XASM:ALWAYS C6XASM:ENC-EXTU-S EMIT                \ lowest kept bit
   19 A 19 A 8 B C6XASM:ENC-OR-L EMIT
   19 A 19 A 20 A C6XASM:ENC-OR-L EMIT
   18 A 18 A 19 A C6XASM:ENC-AND-L EMIT                        \ increment
   20 A 254 C6XASM:ENC-MVK EMIT
   2 A 8 A 20 A C6XASM:ALWAYS C6XASM:ENC-CMPGT-L EMIT
   2 A WHEN FORWARD {: infinite3:n :}                   \ overflow
   8 A 8 A -1 C6XASM:ENC-ADD-I5 1 A C6XASM:WHEN-ZERO EMIT             \ the hidden bit carries one exponent unit
   8 A 8 A 23 C6XASM:ALWAYS C6XASM:ENC-SHL-U5 EMIT
   8 A 8 A 9 A C6XASM:ENC-ADD-L EMIT
   8 A 8 A 18 A C6XASM:ENC-ADD-L EMIT
   4 A 7 A 8 A C6XASM:ENC-OR-L EMIT
   RETURN
   x-nan RESOLVE
   4 A 4 A 22 22 C6XASM:ALWAYS C6XASM:ENC-SET-S EMIT RETURN
   y-nan RESOLVE
   4 A 4 B C6XASM:ENC-MV-L EMIT 4 A 4 A 22 22 C6XASM:ALWAYS C6XASM:ENC-SET-S EMIT RETURN
   invalid RESOLVE invalid2 RESOLVE
   4 A $7FC00000 C6XASM:ENC-MVKL EMIT 4 A $7FC00000 C6XASM:ENC-MVKH EMIT RETURN
   infinite RESOLVE infinite2 RESOLVE infinite3 RESOLVE
   8 A $7F800000 C6XASM:ENC-MVKL EMIT 8 A $7F800000 C6XASM:ENC-MVKH EMIT 4 A 7 A 8 A C6XASM:ENC-OR-L EMIT RETURN
   zero RESOLVE zero2 RESOLVE
   4 A 7 A C6XASM:ENC-MV-L EMIT RETURN ;


\ ---- float64 division: bit-exact IEEE 754, round to nearest even -------------------

\ hi:lo <<= count (0..63), all on one bank; g guards and t is scratch there.
: SHL64 ( C6XASM:gpr C6XASM:gpr C6XASM:gpr C6XASM:gpr C6XASM:gpr -- ) {: hi:C6XASM:gpr lo:C6XASM:gpr count:C6XASM:gpr g:C6XASM:gpr t:C6XASM:gpr :}
   t count -16 C6XASM:ENC-ADD-I5 EMIT
   t t -16 C6XASM:ENC-ADD-I5 EMIT                             \ count - 32
   g t -1 C6XASM:ALWAYS C6XASM:ENC-CMPLT-I5 EMIT                     \ count >= 32
   hi lo t g C6XASM:IF-NONZERO C6XASM:ENC-SHL-S EMIT
   lo 0 C6XASM:ENC-MVK g C6XASM:WHEN-NONZERO EMIT
   t 32 C6XASM:ENC-MVK g C6XASM:WHEN-ZERO EMIT
   t t count C6XASM:ENC-SUB-L g C6XASM:WHEN-ZERO EMIT                \ 32 - count
   t lo t g C6XASM:IF-ZERO C6XASM:ENC-SHRU-S EMIT
   hi hi count g C6XASM:IF-ZERO C6XASM:ENC-SHL-S EMIT
   hi hi t C6XASM:ENC-OR-L g C6XASM:WHEN-ZERO EMIT
   lo lo count g C6XASM:IF-ZERO C6XASM:ENC-SHL-S EMIT ;


\ The left shift that brings a denormal significand's top bit to bit 52, into
\ count; t is scratch on the same bank, g and h are guards.
: DENORMAL-SHIFT ( C6XASM:gpr C6XASM:gpr C6XASM:gpr C6XASM:gpr C6XASM:gpr C6XASM:gpr -- ) {: hi:C6XASM:gpr lo:C6XASM:gpr count:C6XASM:gpr t:C6XASM:gpr g:C6XASM:gpr h:C6XASM:gpr :}
   count hi C6XASM:ALWAYS C6XASM:ENC-NORM-L EMIT
   count count -10 C6XASM:ENC-ADD-I5 EMIT                     \ when the high word holds the top bit
   g hi 0 C6XASM:ALWAYS C6XASM:ENC-CMPLTU-U4 EMIT
   t lo C6XASM:ALWAYS C6XASM:ENC-NORM-L EMIT
   t t 1 C6XASM:ENC-ADD-I5 EMIT                               \ leading zeros of the low word
   h lo 0 C6XASM:ALWAYS C6XASM:ENC-CMPGT-I5 EMIT
   t 0 C6XASM:ENC-MVK h C6XASM:WHEN-NONZERO EMIT
   t t 15 C6XASM:ENC-ADD-I5 EMIT t t 6 C6XASM:ENC-ADD-I5 EMIT        \ plus 21 when the high word is zero
   count t C6XASM:ENC-MV-L g C6XASM:WHEN-ZERO EMIT ;


\ rem (rhi:rlo) < d (dhi:dlo) into lt, with the low-word borrow left in borrow
\ and eq as scratch; rem is on lt's bank.
: LESS64 ( C6XASM:gpr C6XASM:gpr C6XASM:gpr C6XASM:gpr C6XASM:gpr C6XASM:gpr C6XASM:gpr -- ) {: rhi:C6XASM:gpr rlo:C6XASM:gpr dhi:C6XASM:gpr dlo:C6XASM:gpr lt:C6XASM:gpr eq:C6XASM:gpr borrow:C6XASM:gpr :}
   lt rhi dhi C6XASM:ALWAYS C6XASM:ENC-CMPLTU-L EMIT
   eq rhi dhi C6XASM:ENC-CMPEQ-L EMIT
   borrow rlo dlo C6XASM:ALWAYS C6XASM:ENC-CMPLTU-L EMIT
   eq eq borrow C6XASM:ENC-AND-L EMIT
   lt lt eq C6XASM:ENC-OR-L EMIT ;


\ divd(x A5:A4, y B5:B4) under the standard convention, as divf with 53-bit
\ significands held in register pairs and a 54- or 55-step division.
: DIVD ( -- )
   8 A 5 A 1 21 C6XASM:ALWAYS C6XASM:ENC-EXTU-S EMIT                  \ A8 = exponent of x
   8 B 5 B 1 21 C6XASM:ALWAYS C6XASM:ENC-EXTU-S EMIT                  \ B8 = exponent of y
   6 A 5 A 12 12 C6XASM:ALWAYS C6XASM:ENC-EXTU-S EMIT                 \ A6:A16 = fraction of x
   6 B 5 B 12 12 C6XASM:ALWAYS C6XASM:ENC-EXTU-S EMIT                 \ B6:B16 = fraction of y
   16 A 4 A C6XASM:ENC-MV-L EMIT
   16 B 4 B C6XASM:ENC-MV-L EMIT
   7 A 5 A 5 B C6XASM:ENC-XOR-L EMIT                           \ A7 = the result's sign bit
   7 A 7 A 0 30 C6XASM:ALWAYS C6XASM:ENC-CLR-S EMIT
   17 A 2047 C6XASM:ENC-MVK EMIT
   0 A 17 A 8 A C6XASM:ENC-CMPEQ-L EMIT                        \ A0 = x has the top exponent
   1 A 17 A 8 B C6XASM:ENC-CMPEQ-L EMIT                        \ A1 = y has the top exponent
   2 A 6 A 16 A C6XASM:ENC-OR-L EMIT
   2 A 2 A 0 C6XASM:ALWAYS C6XASM:ENC-CMPLTU-U4 EMIT
   2 A 2 A 0 A C6XASM:ENC-AND-L EMIT                           \ x is NaN
   2 A WHEN FORWARD {: x-nan:n :}
   2 B 6 B 16 B C6XASM:ENC-OR-L EMIT
   2 B 2 B 0 C6XASM:ALWAYS C6XASM:ENC-CMPLTU-U4 EMIT
   2 B 2 B 1 A C6XASM:ENC-AND-L EMIT                           \ y is NaN
   2 B WHEN FORWARD {: y-nan:n :}
   2 A 0 A 1 A C6XASM:ENC-AND-L EMIT
   2 A WHEN FORWARD {: invalid:n :}                     \ both infinite
   0 A WHEN FORWARD {: infinite:n :}
   1 A WHEN FORWARD {: zero:n :}
   0 B 8 B 1 C6XASM:ALWAYS C6XASM:ENC-CMPGTU-U4 EMIT                  \ B0 = y has a zero exponent
   1 B 6 B 16 B C6XASM:ENC-OR-L EMIT
   1 B 1 B 1 C6XASM:ALWAYS C6XASM:ENC-CMPGTU-U4 EMIT
   2 B 0 B 1 B C6XASM:ENC-AND-L EMIT                           \ y is zero
   0 A 8 A 1 C6XASM:ALWAYS C6XASM:ENC-CMPGTU-U4 EMIT                  \ A0 = x has a zero exponent
   1 A 6 A 16 A C6XASM:ENC-OR-L EMIT
   1 A 1 A 1 C6XASM:ALWAYS C6XASM:ENC-CMPGTU-U4 EMIT
   2 A 0 A 1 A C6XASM:ENC-AND-L EMIT                           \ x is zero
   1 B 2 B 2 A C6XASM:ENC-AND-L EMIT
   1 B WHEN FORWARD {: invalid2:n :}                    \ 0 / 0
   2 B WHEN FORWARD {: infinite2:n :}                   \ x / 0
   2 A WHEN FORWARD {: zero2:n :}                       \ 0 / y
   6 A 16 A 17 A 22 A 1 A 2 A DENORMAL-SHIFT            \ normalise a denormal x
   17 A 0 C6XASM:ENC-MVK 0 A C6XASM:WHEN-ZERO EMIT
   6 A 16 A 17 A 1 A 23 A SHL64
   8 A 1 C6XASM:ENC-MVK 0 A C6XASM:WHEN-NONZERO EMIT
   8 A 8 A 17 A C6XASM:ENC-SUB-L 0 A C6XASM:WHEN-NONZERO EMIT
   6 A 6 A 20 20 0 A C6XASM:IF-ZERO C6XASM:ENC-SET-S EMIT              \ or restore the hidden bit
   6 B 16 B 7 B 9 B 1 B 2 B DENORMAL-SHIFT              \ the same for y
   7 B 0 C6XASM:ENC-MVK 0 B C6XASM:WHEN-ZERO EMIT
   6 B 16 B 7 B 1 B 17 B SHL64
   8 B 1 C6XASM:ENC-MVK 0 B C6XASM:WHEN-NONZERO EMIT
   8 B 8 B 7 B C6XASM:ENC-SUB-L 0 B C6XASM:WHEN-NONZERO EMIT
   6 B 6 B 20 20 0 B C6XASM:IF-ZERO C6XASM:ENC-SET-S EMIT
   8 A 8 A 8 B C6XASM:ENC-SUB-L EMIT                           \ A8 = ex - ey + 1023
   17 A 1023 C6XASM:ENC-MVK EMIT
   8 A 8 A 17 A C6XASM:ENC-ADD-L EMIT
   6 A 16 A 6 B 16 B 1 A 2 A 0 A LESS64                 \ A1 = mx < my
   9 A 0 C6XASM:ENC-MVK EMIT 20 A 1 C6XASM:ENC-MVK EMIT               \ quotient A9:A20 = 1
   19 A 16 A 16 B C6XASM:ENC-SUB-L EMIT                        \ remainder A18:A19 = mx - my
   18 A 6 A 6 B C6XASM:ENC-SUB-L EMIT
   18 A 18 A 0 A C6XASM:ENC-SUB-L EMIT
   1 B 54 C6XASM:ENC-MVK EMIT
   20 A 0 C6XASM:ENC-MVK 1 A C6XASM:WHEN-NONZERO EMIT                 \ or 0 and mx with one more step
   19 A 16 A C6XASM:ENC-MV-L 1 A C6XASM:WHEN-NONZERO EMIT
   18 A 6 A C6XASM:ENC-MV-L 1 A C6XASM:WHEN-NONZERO EMIT
   8 A 8 A -1 C6XASM:ENC-ADD-I5 1 A C6XASM:WHEN-NONZERO EMIT
   1 B 55 C6XASM:ENC-MVK 1 A C6XASM:WHEN-NONZERO EMIT
   HERE {: loop:n :}
   17 A 19 A 31 C6XASM:ALWAYS C6XASM:ENC-SHRU-U5 EMIT                 \ remainder <<= 1
   18 A 18 A 1 C6XASM:ALWAYS C6XASM:ENC-SHL-U5 EMIT
   18 A 18 A 17 A C6XASM:ENC-OR-L EMIT
   19 A 19 A 1 C6XASM:ALWAYS C6XASM:ENC-SHL-U5 EMIT
   17 A 20 A 31 C6XASM:ALWAYS C6XASM:ENC-SHRU-U5 EMIT                 \ quotient <<= 1
   9 A 9 A 1 C6XASM:ALWAYS C6XASM:ENC-SHL-U5 EMIT
   9 A 9 A 17 A C6XASM:ENC-OR-L EMIT
   20 A 20 A 1 C6XASM:ALWAYS C6XASM:ENC-SHL-U5 EMIT
   18 A 19 A 6 B 16 B 1 A 2 A 0 A LESS64                \ A1 = remainder < my
   19 A 19 A 16 B C6XASM:ENC-SUB-L 1 A C6XASM:WHEN-ZERO EMIT
   18 A 18 A 6 B C6XASM:ENC-SUB-L 1 A C6XASM:WHEN-ZERO EMIT
   18 A 18 A 0 A C6XASM:ENC-SUB-L 1 A C6XASM:WHEN-ZERO EMIT
   20 A 20 A 1 C6XASM:ENC-ADD-I5 1 A C6XASM:WHEN-ZERO EMIT
   1 B 1 B -1 C6XASM:ENC-ADD-I5 EMIT
   loop 1 B WHEN BACK
   21 A 18 A 19 A C6XASM:ENC-OR-L EMIT
   21 A 21 A 0 C6XASM:ALWAYS C6XASM:ENC-CMPLTU-U4 EMIT                \ A21 = sticky
   1 A 8 A 1 C6XASM:ALWAYS C6XASM:ENC-CMPGT-I5 EMIT                   \ A1 = the result is denormal
   17 A 1 C6XASM:ENC-MVK EMIT
   17 A 17 A 8 A C6XASM:ENC-SUB-L EMIT                         \ A17 = extra right shift
   17 A 0 C6XASM:ENC-MVK 1 A C6XASM:WHEN-ZERO EMIT
   22 A 56 C6XASM:ENC-MVK EMIT
   2 A 17 A 22 A C6XASM:ALWAYS C6XASM:ENC-CMPGT-L EMIT
   17 A 56 C6XASM:ENC-MVK 2 A C6XASM:WHEN-NONZERO EMIT
   8 A 0 C6XASM:ENC-MVK 1 A C6XASM:WHEN-NONZERO EMIT
   2 B 17 A 0 C6XASM:ALWAYS C6XASM:ENC-CMPLTU-U4 EMIT
   2 B UNLESS FORWARD {: no-shift:n :}
   HERE {: shifting:n :}
   22 A 20 A 31 31 C6XASM:ALWAYS C6XASM:ENC-EXTU-S EMIT               \ the bit about to leave
   21 A 21 A 22 A C6XASM:ENC-OR-L EMIT
   20 A 20 A 1 C6XASM:ALWAYS C6XASM:ENC-SHRU-U5 EMIT
   22 A 9 A 31 C6XASM:ALWAYS C6XASM:ENC-SHL-U5 EMIT
   20 A 20 A 22 A C6XASM:ENC-OR-L EMIT
   9 A 9 A 1 C6XASM:ALWAYS C6XASM:ENC-SHRU-U5 EMIT
   17 A 17 A -1 C6XASM:ENC-ADD-I5 EMIT
   2 B 17 A 0 C6XASM:ALWAYS C6XASM:ENC-CMPLTU-U4 EMIT
   shifting 2 B WHEN BACK
   no-shift RESOLVE
   22 A 20 A 30 31 C6XASM:ALWAYS C6XASM:ENC-EXTU-S EMIT               \ guard
   23 A 20 A 31 31 C6XASM:ALWAYS C6XASM:ENC-EXTU-S EMIT               \ round
   20 A 20 A 2 C6XASM:ALWAYS C6XASM:ENC-SHRU-U5 EMIT                  \ quotient >>= 2
   17 A 9 A 30 C6XASM:ALWAYS C6XASM:ENC-SHL-U5 EMIT
   20 A 20 A 17 A C6XASM:ENC-OR-L EMIT
   9 A 9 A 2 C6XASM:ALWAYS C6XASM:ENC-SHRU-U5 EMIT
   17 A 20 A 31 31 C6XASM:ALWAYS C6XASM:ENC-EXTU-S EMIT               \ lowest kept bit
   23 A 23 A 21 A C6XASM:ENC-OR-L EMIT
   23 A 23 A 17 A C6XASM:ENC-OR-L EMIT
   22 A 22 A 23 A C6XASM:ENC-AND-L EMIT                        \ increment
   17 A 2046 C6XASM:ENC-MVK EMIT
   2 A 8 A 17 A C6XASM:ALWAYS C6XASM:ENC-CMPGT-L EMIT
   2 A WHEN FORWARD {: infinite3:n :}                   \ overflow
   20 A 20 A 22 A C6XASM:ENC-ADD-L EMIT
   17 A 20 A 1 C6XASM:ALWAYS C6XASM:ENC-CMPGTU-U4 EMIT                \ the low word wrapped to zero
   17 A 17 A 22 A C6XASM:ENC-AND-L EMIT
   9 A 9 A 17 A C6XASM:ENC-ADD-L EMIT
   8 A 8 A -1 C6XASM:ENC-ADD-I5 1 A C6XASM:WHEN-ZERO EMIT             \ the hidden bit carries one exponent unit
   8 A 8 A 20 C6XASM:ALWAYS C6XASM:ENC-SHL-U5 EMIT
   9 A 9 A 8 A C6XASM:ENC-ADD-L EMIT
   5 A 7 A 9 A C6XASM:ENC-OR-L EMIT
   4 A 20 A C6XASM:ENC-MV-L EMIT
   RETURN
   x-nan RESOLVE
   5 A 5 A 19 19 C6XASM:ALWAYS C6XASM:ENC-SET-S EMIT RETURN
   y-nan RESOLVE
   4 A 4 B C6XASM:ENC-MV-L EMIT 5 A 5 B C6XASM:ENC-MV-L EMIT 5 A 5 A 19 19 C6XASM:ALWAYS C6XASM:ENC-SET-S EMIT RETURN
   invalid RESOLVE invalid2 RESOLVE
   5 A $7FF80000 C6XASM:ENC-MVKL EMIT 5 A $7FF80000 C6XASM:ENC-MVKH EMIT 4 A 0 C6XASM:ENC-MVK EMIT RETURN
   infinite RESOLVE infinite2 RESOLVE infinite3 RESOLVE
   9 A $7FF00000 C6XASM:ENC-MVKL EMIT 9 A $7FF00000 C6XASM:ENC-MVKH EMIT 5 A 7 A 9 A C6XASM:ENC-OR-L EMIT 4 A 0 C6XASM:ENC-MVK EMIT RETURN
   zero RESOLVE zero2 RESOLVE
   5 A 7 A C6XASM:ENC-MV-L EMIT 4 A 0 C6XASM:ENC-MVK EMIT RETURN ;


\ ---- register sets (SPRAB89 Table 8-9; divremu adds A5, which the contract returns) ----

: A-BIT ( n -- n ) 1 swap lshift ;
: B-BIT ( n -- n ) 32 + 1 swap lshift ;
: COMMON ( -- n ) 1 A-BIT 4 A-BIT or 0 B-BIT or 1 B-BIT or 2 B-BIT or 4 B-BIT or 30 B-BIT or 31 B-BIT or ;
: CALLER-SAVED ( -- n )
   0 32 0 ?do i 10 < i 15 > or if i A-BIT or i B-BIT or then loop ;

: PERMITTED! ( -- )
   COMMON 0 A-BIT or 2 A-BIT or 6 A-BIT or 5 B-BIT or PERMITTED 0 cells + !          \ divi
   COMMON 0 A-BIT or 2 A-BIT or 6 A-BIT or PERMITTED 1 cells + !                      \ divu
   COMMON 2 A-BIT or 5 A-BIT or 6 A-BIT or PERMITTED 2 cells + !                      \ remi
   COMMON 5 A-BIT or 7 A-BIT or PERMITTED 3 cells + !                                 \ remu
   COMMON 2 A-BIT or 5 A-BIT or 6 A-BIT or PERMITTED 4 cells + !                      \ divremi
   COMMON 0 A-BIT or 2 A-BIT or 5 A-BIT or 6 A-BIT or PERMITTED 5 cells + !           \ divremu
   CALLER-SAVED PERMITTED 6 cells + ! CALLER-SAVED PERMITTED 7 cells + !
   CALLER-SAVED PERMITTED 8 cells + ! CALLER-SAVED PERMITTED 9 cells + ! ;
PERMITTED!

public

: HELPER-NAME$ ( n -- str ) {: idx:n :}
   idx 0 = if s" __c6xabi_divi" exit then
   idx 1 = if s" __c6xabi_divu" exit then
   idx 2 = if s" __c6xabi_remi" exit then
   idx 3 = if s" __c6xabi_remu" exit then
   idx 4 = if s" __c6xabi_divremi" exit then
   idx 5 = if s" __c6xabi_divremu" exit then
   idx 6 = if s" memcpy" exit then
   idx 7 = if s" memset" exit then
   idx 8 = if s" __c6xabi_divf" exit then
   idx 9 = if s" __c6xabi_divd" exit then
   E-OPERAND throw ;


: HELPER ( n -- ) {: idx:n :}
   idx 0 = if DIVI exit then
   idx 1 = if DIVU exit then
   idx 2 = if REMI exit then
   idx 3 = if REMU exit then
   idx 4 = if DIVREMI exit then
   idx 5 = if DIVREMU exit then
   idx 6 = if MEMCPY exit then
   idx 7 = if MEMSET exit then
   idx 8 = if DIVF exit then
   idx 9 = if DIVD exit then
   E-OPERAND throw ;

\ Emits helper idx into the program buffer, packed; PROGRAM$ reads it back.
: EMIT-HELPER ( n -- ) {: idx:n :}
   0 LEN ! 0 FIXUP-COUNT ! 0 PENDING-COUNT !
   idx HELPER FLUSH RESOLVED ;

: PROGRAM$ ( -- ^n n ) WORDS LEN @ ;

\ TRUE when helper idx may modify the register with that code (A0..A31 = 0..31,
\ B0..B31 = 32..63); B3 is the return address and is never checked.
: PERMITTED? ( n n -- bool ) {: idx:n code:n :}
   idx 0 < idx HELPER-COUNT >= or if E-OPERAND throw then
   code 0 < code 63 > or if E-OPERAND throw then
   PERMITTED idx cells + @ 1 code lshift and 0 <> ;

;package

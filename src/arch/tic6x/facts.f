\ Resource and dependence facts of one instruction word from the C66x subset
\ that src/arch/tic6x/asm.f constructs: the functional unit it occupies, the
\ register it reads through a cross path, the registers it reads, the registers
\ it writes at the end of its cycle and the ones a load fills after four delay
\ slots, the register file its memory data moves on, and whether it branches
\ or idles. The emitter's packet scheduler and the interpreter's packet checks
\ both read these (SPRUGH7 section 3.8), so a word outside the subset is
\ refused here rather than misjudged. The p bit is packet structure, not an
\ instruction fact, and is ignored.
require lib/errors.f

package C6XFACTS
public

E-C6XFACTS-DECODE constant E-DECODE
0 constant UNIT-L
1 constant UNIT-S
2 constant UNIT-D
5 constant LOAD-LATENCY                     \ cycles from a load's issue until its value is readable

: MASK-BIT ( n -- n ) 1 swap lshift ;       \ register masks: A0..A31 bits 0..31, B0..B31 bits 32..63

\ What one instruction occupies and touches. unit is the unit class times two
\ plus its side, or -1 for a NOP; cross is the register read through the
\ cross path, or -1; reads, writes and loads are register masks, writes
\ landing at the end of the cycle and loads after four delay slots; memory is
\ 0, 1 for a load or 2 for a store, and side the register file its data moves
\ on, or -1; idle is a NOP's count, else 0.
STRUCTURE facts 0
   FIELD unit n
   FIELD cross n
   FIELD reads n
   FIELD writes n
   FIELD loads n
   FIELD memory n
   FIELD side n
   FIELD branch bool
   FIELD idle n
;STRUCTURE

private

variable CURRENT
variable UNIT
variable CROSS
variable READS
variable WRITES
variable LOADS
variable MEMORY
variable DATA-SIDE
variable BRANCHES
variable IDLE

: BITS ( n n -- n ) {: low:n width:n :} CURRENT @ low rshift 1 width lshift 1- and ;
: S-BIT ( -- n ) 1 1 BITS ;
: X-BIT ( -- n ) 12 1 BITS ;
: DST ( -- n ) 23 5 BITS S-BIT 32 * + ;
: SRC1 ( -- n ) 13 5 BITS S-BIT 32 * + ;
: SRC2 ( -- n ) 18 5 BITS S-BIT 32 * + ;                \ on the unit's side

: READ-REG ( n -- ) MASK-BIT READS @ or READS ! ;
: WRITE-REG ( n -- ) MASK-BIT WRITES @ or WRITES ! ;
: LOAD-REG ( n -- ) MASK-BIT LOADS @ or LOADS ! ;
: USE-UNIT ( n n -- ) {: kind:n side:n :} kind 2 * side + UNIT ! ;

\ src2 sits on the unit's side or, with the x bit, comes through the cross path.
: READ-SRC2 ( -- )
   X-BIT 0= if SRC2 READ-REG exit then
   18 5 BITS S-BIT 1 xor 32 * + dup READ-REG CROSS ! ;

\ A predicated instruction reads its guard: creg 1..3 are B0..B2, 4 and 5 are
\ A1 and A2, and 6 is A0.
: READ-GUARD ( -- )
   29 3 BITS {: creg:n :}
   creg 0= if exit then
   creg 7 = if E-DECODE throw then
   creg 4 < if creg 1- 32 + else creg 6 = if 0 else creg 3 - then then READ-REG ;

: L-FORM ( -- )
   5 7 BITS {: op:n :}
   UNIT-L S-BIT USE-UNIT DST WRITE-REG READ-SRC2
   op 2 = op 70 = or op 78 = or op 86 = or op 94 = or op 126 = or op 26 = or op 99 = or if exit then
   op 3 = op 7 = or op 123 = or op 127 = or op 111 = or op 83 = or op 75 = or
   op 71 = or op 79 = or op 87 = or op 95 = or if SRC1 READ-REG exit then
   E-DECODE throw ;

: S-FORM ( -- )
   6 6 BITS {: op:n :}
   UNIT-S S-BIT USE-UNIT DST WRITE-REG READ-SRC2
   op 50 = op 54 = or op 38 = or if exit then
   op 51 = op 55 = or op 39 = or if SRC1 READ-REG exit then
   E-DECODE throw ;

: FIELD-FORM ( -- ) UNIT-S S-BIT USE-UNIT DST WRITE-REG SRC2 READ-REG ;

: D-FORM ( -- )
   7 6 BITS {: op:n :}
   UNIT-D S-BIT USE-UNIT DST WRITE-REG SRC2 READ-REG
   op 50 = op 58 = or if exit then
   op 48 = op 56 = or if SRC1 READ-REG exit then
   E-DECODE throw ;

\ MVKH and MVKLH keep the low half of their destination.
: MOVE-FORM ( -- ) UNIT-S S-BIT USE-UNIT DST WRITE-REG 6 1 BITS 0 <> if DST READ-REG then ;

: BRANCH-REG ( -- )
   UNIT-S 1 USE-UNIT 1 BRANCHES !
   X-BIT 0= if 18 5 BITS 32 + READ-REG exit then
   18 5 BITS dup READ-REG CROSS ! ;

: BRANCH-REL ( -- ) UNIT-S S-BIT USE-UNIT 1 BRANCHES ! ;

: MEMORY-FORM ( -- )
   4 3 BITS 8 1 BITS 3 lshift or {: kind:n :}
   9 4 BITS {: mode:n :}
   7 1 BITS {: side:n :}
   side 32 * 18 5 BITS + {: base:n :}
   DST {: data:n :}
   mode 4 and 0 <> if E-DECODE throw then                \ register offsets
   mode 8 and 0 <> mode 2 and 0= and if E-DECODE throw then   \ pre-modification
   UNIT-D side USE-UNIT base READ-REG S-BIT DATA-SIDE !
   mode 8 and 0 <> if base WRITE-REG then
   kind 1 = kind 2 = or kind 6 = or if data LOAD-REG 1 MEMORY ! exit then
   kind 14 = if data LOAD-REG data 1+ LOAD-REG 1 MEMORY ! exit then
   kind 3 = kind 7 = or if data READ-REG 2 MEMORY ! exit then
   kind 12 = if data READ-REG data 1+ READ-REG 2 MEMORY ! exit then
   E-DECODE throw ;

\ Decodes CURRENT into the accumulators by its form.
: DECODE ( -- )
   CURRENT @ $FFFE1FFF and 0= if 13 4 BITS 1+ IDLE ! exit then
   READ-GUARD
   CURRENT @ $7C and $28 = CURRENT @ $7C and $68 = or if MOVE-FORM exit then
   CURRENT @ $FFF and $362 = if BRANCH-REG exit then
   CURRENT @ $1C and $18 = if L-FORM exit then
   CURRENT @ $3C and $20 = if S-FORM exit then
   CURRENT @ $3C and $08 = if FIELD-FORM exit then
   CURRENT @ $7C and $40 = if D-FORM exit then
   CURRENT @ $7C and $10 = if BRANCH-REL exit then
   CURRENT @ $0C and $04 = if MEMORY-FORM exit then
   E-DECODE throw ;

public

\ The facts of one instruction word.
: CLASSIFY ( n -- facts ) {: w:n :}
   w $FFFFFFFE and CURRENT !
   -1 UNIT ! -1 CROSS ! 0 READS ! 0 WRITES ! 0 LOADS ! 0 MEMORY ! -1 DATA-SIDE ! 0 BRANCHES ! 0 IDLE !
   DECODE
   UNIT @ CROSS @ READS @ WRITES @ LOADS @ MEMORY @ DATA-SIDE @ BRANCHES @ 0 <> IDLE @ C6XFACTS-FACTS:MAKE ;

;package

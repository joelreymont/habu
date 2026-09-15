\ A host interpreter for the C66x subset that src/arch/tic6x/asm.f constructs:
\ one instruction per execute packet, load results after four delay slots,
\ branches after five, predication, and a small byte-addressed memory window.
\ It exists to run emitted helper programs on the host against an oracle before
\ the board does; it models no pipeline hazards beyond those delays, no
\ parallel packets, and no instruction outside the encoder's subset.
require lib/errors.f
require src/arch/tic6x/asm.f

package C6XSIM
public

E-C6XSIM-DECODE constant E-DECODE
E-C6XSIM-FAULT constant E-FAULT
E-C6XSIM-LIMIT constant E-LIMIT
E-C6XSIM-OPERAND constant E-OPERAND
$00800000 constant MEMORY-BASE
$10000 constant MEMORY-BYTES
$FFFFFFE0 constant RETURN-ADDRESS          \ what B3 holds when CALL starts

private

$FFFFFFFF constant MASK32
4 constant LOAD-DELAY
5 constant BRANCH-DELAY
8 constant PENDING-MAX
create REGS 64 cells allot
create MEMORY MEMORY-BYTES allot
create PENDING PENDING-MAX 3 * cells allot   \ register, value, cycles left
variable PENDING-COUNT
variable PROGRAM
variable PROGRAM-WORDS
variable NEXT-WORD                                 \ word index of the next instruction
variable CYCLES
variable BUDGET
variable BRANCH-PENDING
variable BRANCH-TARGET
variable BRANCH-LEFT
variable CURRENT
variable SCAN
variable MAGNITUDE
variable DONE

: RANGE ( n n n -- n ) {: value:n low:n high:n :}
   value low < value high > or if E-OPERAND throw then value ;


: >U32 ( n -- n ) MASK32 and ;
: >S32 ( n -- n ) >U32 dup $80000000 and 0 <> if $100000000 - then ;
: SEXT ( n n -- n ) {: value:n bits:n :}
   value 1 bits lshift 1- and dup 1 bits 1- lshift and 0 <> if 1 bits lshift - then ;


: REG-CELL ( n -- ptr a ) dup 0 < over 63 > or if E-OPERAND throw then cells REGS + ;
: REG@ ( n -- n ) REG-CELL @ ;
: REG! ( n n -- ) swap >U32 swap REG-CELL ! ;


: ADDRESS ( n -- ptr u8 ) {: address:n :}
   address MEMORY-BASE - dup 0 < over MEMORY-BYTES >= or if E-FAULT throw then MEMORY + ;
: BYTE@ ( n -- n ) ADDRESS c@ ;
: BYTE! ( n n -- ) swap 255 and swap ADDRESS c! ;
: WORD@ ( n -- n ) {: address:n :}
   address 3 and 0 <> if E-FAULT throw then
   address BYTE@ address 1+ BYTE@ 8 lshift or address 2 + BYTE@ 16 lshift or address 3 + BYTE@ 24 lshift or ;
: WORD! ( n n -- ) {: value:n address:n :}
   address 3 and 0 <> if E-FAULT throw then
   value address BYTE! value 8 rshift address 1+ BYTE! value 16 rshift address 2 + BYTE! value 24 rshift address 3 + BYTE! ;


\ ---- delayed register writes ------------------------------------------------------

: PENDING-ROW ( n -- ptr a ) 3 * cells PENDING + ;

: SCHEDULE ( n n n -- ) {: reg:n value:n delay:n :}
   PENDING-COUNT @ PENDING-MAX >= if E-LIMIT throw then
   PENDING-COUNT @ PENDING-ROW {: row:ptr :}
   reg row ! value >U32 row 1 cells + ! delay 1+ row 2 cells + !
   1 PENDING-COUNT +! ;

\ Advances every pending write by one cycle, landing those that are due.
: SETTLE ( -- )
   0 SCAN !
   begin SCAN @ PENDING-COUNT @ < while
      SCAN @ PENDING-ROW {: row:ptr :}
      row 2 cells + @ 1- dup row 2 cells + !
      0= if
         row 1 cells + @ row @ REG!
         PENDING-COUNT @ 1- PENDING-ROW {: last:ptr :}
         last @ row ! last 1 cells + @ row 1 cells + ! last 2 cells + @ row 2 cells + !
         -1 PENDING-COUNT +!
      else 1 SCAN +! then
   repeat ;


\ ---- decoding -------------------------------------------------------------------------

: UNMODELLED ( -- ) s" c6xsim: cannot model word " type CURRENT @ . cr E-DECODE throw ;

: BITS ( n n -- n ) {: low:n width:n :} CURRENT @ low rshift 1 width lshift 1- and ;
: S-BIT ( -- n ) 1 1 BITS ;
: DST ( -- n ) 23 5 BITS S-BIT 32 * + ;
: SRC2 ( -- n ) 18 5 BITS S-BIT 12 1 BITS xor 32 * + ;       \ x selects the cross path
: SRC2-SAME ( -- n ) 18 5 BITS S-BIT 32 * + ;                 \ forms without a cross path
: SRC1 ( -- n ) 13 5 BITS S-BIT 32 * + ;
: SRC1-SIGNED ( -- n ) 13 5 BITS 5 SEXT ;
: SRC1-UNSIGNED ( -- n ) 13 5 BITS ;


: CONDITION-TRUE? ( -- bool )
   29 3 BITS {: creg:n :}
   creg 0= if TRUE exit then
   creg 1 = if 32 else creg 2 = if 33 else creg 3 = if 34 else
   creg 4 = if 1 else creg 5 = if 2 else creg 6 = if 0 else UNMODELLED then then then then then then
   REG@ 0 <> 28 1 BITS 0 <> xor ;


\ The number of redundant sign bits: leading bits equal to bit 31, less one.
: NORM ( n -- n ) {: value:n :}
   value >S32 0 < if value >U32 MASK32 xor else value >U32 then MAGNITUDE !
   0 SCAN !
   begin SCAN @ 31 < MAGNITUDE @ $40000000 and 0= and while
      1 SCAN +! MAGNITUDE @ 1 lshift >U32 MAGNITUDE !
   repeat SCAN @ ;


: SHIFT-SCAN ( n -- n ) 63 and ;                              \ counts above 31 clear the result
: SHL ( n n -- n ) {: value:n count:n :} count SHIFT-SCAN 31 > if 0 else value count SHIFT-SCAN lshift >U32 then ;
: SHRU ( n n -- n ) {: value:n count:n :} count SHIFT-SCAN 31 > if 0 else value >U32 count SHIFT-SCAN rshift then ;
: SHR ( n n -- n ) {: value:n count:n :}
   count SHIFT-SCAN 31 > if value >S32 0 < if MASK32 else 0 then exit then
   value >S32 count SHIFT-SCAN rshift >U32 ;
: FIELD-MASK ( n n -- n ) {: low:n high:n :} 1 high 1+ lshift 1- 1 low lshift 1- xor >U32 ;


\ ---- execution --------------------------------------------------------------------------

: BRANCH ( n -- ) {: target:n :}
   BRANCH-PENDING @ 0 <> if UNMODELLED then
   target BRANCH-TARGET ! 1 BRANCH-PENDING ! BRANCH-DELAY BRANCH-LEFT ! ;


\ .L unit forms: three-operand, constant and two-operand, by op field.
: EXECUTE-L ( -- )
   5 7 BITS {: op:n :}
   DST {: dst:n :}
   SRC2 REG@ {: b:n :}
   op 3 = if SRC1 REG@ b + dst REG! exit then
   op 2 = if SRC1-SIGNED b + dst REG! exit then
   op 7 = if SRC1 REG@ b - dst REG! exit then
   op 123 = if SRC1 REG@ b and dst REG! exit then
   op 127 = if SRC1 REG@ b or dst REG! exit then
   op 126 = if b dst REG! exit then
   op 111 = if SRC1 REG@ b xor dst REG! exit then
   op 83 = if SRC1 REG@ b = if 1 else 0 then dst REG! exit then
   op 75 = if                                            \ SUBC compares unsigned operands
      SRC1 REG@ {: a:n :}
      a b >= if a b - 1 lshift 1 or else a 1 lshift then dst REG! exit
   then
   op 71 = if SRC1 REG@ >S32 b >S32 > if 1 else 0 then dst REG! exit then
   op 70 = if SRC1-SIGNED b >S32 > if 1 else 0 then dst REG! exit then
   op 79 = if SRC1 REG@ b > if 1 else 0 then dst REG! exit then
   op 78 = if SRC1-UNSIGNED b > if 1 else 0 then dst REG! exit then
   op 87 = if SRC1 REG@ >S32 b >S32 < if 1 else 0 then dst REG! exit then
   op 86 = if SRC1-SIGNED b >S32 < if 1 else 0 then dst REG! exit then
   op 95 = if SRC1 REG@ b < if 1 else 0 then dst REG! exit then
   op 94 = if SRC1-UNSIGNED b < if 1 else 0 then dst REG! exit then
   op 26 = if b >S32 abs dst REG! exit then
   op 99 = if b NORM dst REG! exit then
   UNMODELLED ;


: EXECUTE-S ( -- )
   6 6 BITS {: op:n :}
   DST {: dst:n :}
   SRC2 REG@ {: value:n :}
   op 51 = if value SRC1 REG@ SHL dst REG! exit then
   op 50 = if value SRC1-UNSIGNED SHL dst REG! exit then
   op 55 = if value SRC1 REG@ SHR dst REG! exit then
   op 54 = if value SRC1-UNSIGNED SHR dst REG! exit then
   op 39 = if value SRC1 REG@ SHRU dst REG! exit then
   op 38 = if value SRC1-UNSIGNED SHRU dst REG! exit then
   UNMODELLED ;


: EXECUTE-FIELD ( -- )
   6 2 BITS {: kind:n :}
   DST {: dst:n :}
   SRC2-SAME REG@ {: value:n :}
   13 5 BITS {: csta:n :}
   8 5 BITS {: cstb:n :}
   kind 0 = if value csta SHL cstb SHRU dst REG! exit then
   kind 1 = if value csta SHL cstb SHR dst REG! exit then
   cstb csta < if UNMODELLED then
   kind 2 = if value csta cstb FIELD-MASK or dst REG! exit then
   value csta cstb FIELD-MASK MASK32 xor and dst REG! ;


: EXECUTE-D ( -- )
   7 6 BITS {: op:n :}
   DST {: dst:n :}
   SRC2-SAME REG@ {: base:n :}
   op 48 = if SRC1 REG@ base + dst REG! exit then
   op 50 = if SRC1-UNSIGNED base + dst REG! exit then
   op 56 = if SRC1 REG@ 4 * base + dst REG! exit then
   op 58 = if SRC1-UNSIGNED 4 * base + dst REG! exit then
   UNMODELLED ;


\ Loads and stores: mode bits select offset or post-modification and its sign.
: EXECUTE-MEMORY ( -- )
   4 3 BITS 8 1 BITS 3 lshift or {: kind:n :}
   9 4 BITS {: mode:n :}
   7 1 BITS 32 * 18 5 BITS + {: base:n :}
   13 5 BITS {: offset:n :}
   DST {: data:n :}
   kind 6 = kind 7 = or if 4 else kind 2 = kind 1 = or kind 3 = or if 1 else 8 then then {: scale:n :}
   mode 1 and 0 <> if offset scale * else offset scale * negate then {: delta:n :}
   mode 4 and 0 <> if UNMODELLED then                    \ register offsets are not modelled
   mode 8 and 0 <> if
      mode 2 and 0= if UNMODELLED then                   \ pre-modification is not modelled
      base REG@ {: address:n :} address delta + base REG!
      address
   else base REG@ delta + then {: address:n :}
   kind 2 = if data address BYTE@ 8 SEXT LOAD-DELAY SCHEDULE exit then
   kind 1 = if data address BYTE@ LOAD-DELAY SCHEDULE exit then
   kind 3 = if data REG@ address BYTE! exit then
   kind 6 = if data address WORD@ LOAD-DELAY SCHEDULE exit then
   kind 7 = if data REG@ address WORD! exit then
   kind 14 = if
      data address WORD@ LOAD-DELAY SCHEDULE data 1+ address 4 + WORD@ LOAD-DELAY SCHEDULE exit
   then
   kind 12 = if data REG@ address WORD! data 1+ REG@ address 4 + WORD! exit then
   UNMODELLED ;


: EXECUTE-MOVE ( -- )
   7 16 BITS {: value:n :}
   DST {: dst:n :}
   6 1 BITS 0 <> if dst REG@ $FFFF and value 16 lshift or dst REG! exit then
   value 16 SEXT dst REG! ;


\ Executes the word at NEXT-WORD as one execute packet; NOPs take their cycle count.
: STEP ( -- )
   NEXT-WORD @ 0 < NEXT-WORD @ PROGRAM-WORDS @ >= or if E-FAULT throw then
   PROGRAM @ {: base:ptr :}
   base NEXT-WORD @ cells + @ >U32 CURRENT !
   1 NEXT-WORD +!
   CURRENT @ $FFFE1FFF and 0= if
      13 4 BITS 1+ {: ticks:n :}
      ticks 0 ?do SETTLE 1 CYCLES +! BRANCH-PENDING @ 0 <> if -1 BRANCH-LEFT +! then loop
      exit
   then
   SETTLE 1 CYCLES +!
   BRANCH-PENDING @ 0 <> if -1 BRANCH-LEFT +! then
   CONDITION-TRUE? 0= if exit then
   CURRENT @ $7C and $28 = CURRENT @ $7C and $68 = or if EXECUTE-MOVE exit then
   CURRENT @ $FFF and $362 = if 18 5 BITS 12 1 BITS 1 xor 32 * + REG@ BRANCH exit then
   CURRENT @ $1C and $18 = if EXECUTE-L exit then
   CURRENT @ $3C and $20 = if EXECUTE-S exit then
   CURRENT @ $3C and $08 = if EXECUTE-FIELD exit then
   CURRENT @ $7C and $40 = if EXECUTE-D exit then
   CURRENT @ $7C and $10 = if
      7 21 BITS 21 SEXT 4 * NEXT-WORD @ 1- 4 * $FFFFFFE0 and + BRANCH exit
   then
   CURRENT @ $0C and $04 = if EXECUTE-MEMORY exit then
   UNMODELLED ;

public

: RESET ( -- )
   64 0 ?do 0 i REG! loop
   MEMORY-BYTES 0 ?do 0 MEMORY i + c! loop
   0 PENDING-COUNT ! 0 CYCLES ! 0 BRANCH-PENDING ! 100000 BUDGET ! ;


: A! ( n n -- ) {: value:n index:n :} value index 0 31 RANGE REG! ;
: B! ( n n -- ) {: value:n index:n :} value index 0 31 RANGE 32 + REG! ;
: A@ ( n -- n ) 0 31 RANGE REG@ ;
: B@ ( n -- n ) 0 31 RANGE 32 + REG@ ;
: BUDGET! ( n -- ) BUDGET ! ;
: CYCLES@ ( -- n ) CYCLES @ ;
: MEMORY@ ( n -- n ) BYTE@ ;
: MEMORY! ( n n -- ) BYTE! ;
: MEMORY-WORD@ ( n -- n ) WORD@ ;
: MEMORY-WORD! ( n n -- ) WORD! ;


\ Runs the program from its first word with B3 set to RETURN-ADDRESS until the
\ program branches there and its delay slots have drained; returns the cycles used.
: CALL ( ptr a n -- n ) {: words:ptr count:n :}
   count 0 <= if E-OPERAND throw then
   words PROGRAM ! count PROGRAM-WORDS ! 0 NEXT-WORD ! 0 CYCLES ! 0 BRANCH-PENDING ! 0 PENDING-COUNT !
   RETURN-ADDRESS 3 B! 0 DONE !
   begin DONE @ 0= while
      STEP
      BRANCH-PENDING @ 0 <> BRANCH-LEFT @ 0 <= and if
         0 BRANCH-PENDING !
         BRANCH-TARGET @ RETURN-ADDRESS = if
            begin PENDING-COUNT @ 0 > while SETTLE 1 CYCLES +! repeat
            1 DONE !
         else
            BRANCH-TARGET @ 4 mod 0 <> if E-FAULT throw then
            BRANCH-TARGET @ 4 / NEXT-WORD !
         then
      then
      CYCLES @ BUDGET @ > if E-LIMIT throw then
   repeat
   CYCLES @ ;

;package

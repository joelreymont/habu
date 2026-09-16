\ A host interpreter for the C66x subset that src/arch/tic6x/asm.f constructs:
\ execute packets of up to eight instructions that all read before any writes,
\ load results after four delay slots, branches after five, predication,
\ multicycle NOPs, cross path stalls, and a small byte-addressed memory window.
\ It exists to run emitted helper programs on the host against an oracle before
\ the board does, and it refuses what SPRUGH7 section 3.8 forbids: two
\ instructions on one unit, two registers through one cross path, two memory
\ accesses moving data on one register file, two multicycle NOPs in a packet,
\ two taken branches in a cycle, and two writes landing on one register in one
\ cycle. It models no other hazard and no instruction outside the subset.
require lib/errors.f
require src/arch/tic6x/asm.f
require src/arch/tic6x/facts.f

package C6XSIM
public

E-C6XSIM-DECODE constant E-DECODE
E-C6XSIM-FAULT constant E-FAULT
E-C6XSIM-LIMIT constant E-LIMIT
E-C6XSIM-OPERAND constant E-OPERAND
E-C6XSIM-CONFLICT constant E-CONFLICT
$00800000 constant MEMORY-BASE
$10000 constant MEMORY-BYTES
$FFFFFFE0 constant RETURN-ADDRESS          \ what B3 holds when CALL starts

private
using C6XFACTS

$FFFFFFFF constant MASK32
4 constant LOAD-DELAY
5 constant BRANCH-DELAY
32 constant PENDING-MAX
8 constant PACKET-MAX
16 constant COMMIT-MAX
16 constant STORE-MAX
create REGS 64 cells allot
create MEMORY MEMORY-BYTES allot
create PENDING PENDING-MAX 3 * cells allot   \ register, value, cycles left
create PACKET PACKET-MAX cells allot
create COMMITS COMMIT-MAX 2 * cells allot    \ register, value: written when the packet ends
create STORES STORE-MAX 3 * cells allot      \ address, size, value: written when the packet ends
variable PENDING-COUNT
variable PACKET-COUNT
variable COMMIT-COUNT
variable STORE-COUNT
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
variable MORE
variable LANDED                                    \ registers whose loads landed this cycle
variable WRITTEN-LAST                              \ registers the previous packet wrote
variable RECENT                                    \ the same, for the cross path stall check
variable CROSS-READS                               \ registers this packet reads through cross paths
variable UNITS-USED
variable CROSS-1X                                  \ the B register an A-side unit reads, or -1
variable CROSS-2X                                  \ the A register a B-side unit reads, or -1
variable MEMORY-SIDES
variable IDLE-TICKS                                \ the packet's multicycle NOP count, else 0
variable TAKEN
variable JUMPED
variable CHECKING                                  \ the word under classification

: RANGE ( n n n -- n ) {: value:n low:n high:n :}
   value low < value high > or if E-OPERAND throw then value ;


: >U32 ( n -- n ) MASK32 and ;
: >S32 ( n -- n ) >U32 dup $80000000 and 0 <> if $100000000 - then ;
: SEXT ( n n -- n ) {: value:n bits:n :}
   value 1 bits lshift 1- and dup 1 bits 1- lshift and 0 <> if 1 bits lshift - then ;


: REG-CELL ( n -- ptr n ) dup 0 < over 63 > or if E-OPERAND throw then cells REGS + CELL-VIEW ;
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


\ ---- writes -------------------------------------------------------------------------------
\ A packet's instructions read the registers and memory as they stand; their
\ results queue here and land together when the packet ends, loads later.

: RESULT ( n n -- ) {: value:n reg:n :}
   COMMIT-COUNT @ COMMIT-MAX >= if E-LIMIT throw then
   COMMITS COMMIT-COUNT @ 2 * cells + {: row:ptr :}
   reg row ! value >U32 row 1 cells + !
   1 COMMIT-COUNT +! ;

: STORE ( n n n -- ) {: value:n address:n size:n :}
   STORE-COUNT @ STORE-MAX >= if E-LIMIT throw then
   STORES STORE-COUNT @ 3 * cells + {: row:ptr :}
   address row ! size row 1 cells + ! value row 2 cells + !
   1 STORE-COUNT +! ;

: PENDING-ROW ( n -- ptr n ) 3 * cells PENDING + CELL-VIEW ;

: SCHEDULE ( n n n -- ) {: reg:n value:n delay:n :}
   PENDING-COUNT @ PENDING-MAX >= if E-LIMIT throw then
   PENDING-COUNT @ PENDING-ROW {: row:ptr :}
   reg row ! value >U32 row 1 cells + ! delay 1+ row 2 cells + !
   1 PENDING-COUNT +! ;

\ A load landing on a register another write reached in the same cycle is undefined.
: LAND ( ptr n -- ) {: row:ptr :}
   row @ {: reg:n :}
   LANDED @ reg MASK-BIT and 0 <> WRITTEN-LAST @ reg MASK-BIT and 0 <> or if E-CONFLICT throw then
   LANDED @ reg MASK-BIT or LANDED !
   row 1 cells + @ reg REG! ;

\ Advances every pending load by one cycle, landing those that are due.
: SETTLE ( -- )
   0 LANDED ! 0 SCAN !
   begin SCAN @ PENDING-COUNT @ < while
      SCAN @ PENDING-ROW {: row:ptr :}
      row 2 cells + @ 1- dup row 2 cells + !
      0= if
         row LAND
         PENDING-COUNT @ 1- PENDING-ROW {: last:ptr :}
         last @ row ! last 1 cells + @ row 1 cells + ! last 2 cells + @ row 2 cells + !
         -1 PENDING-COUNT +!
      else 1 SCAN +! then
   repeat ;

: COMMIT-ROW ( n -- ) {: idx:n :}
   COMMITS idx 2 * cells + {: row:ptr :}
   row @ {: reg:n :}
   WRITTEN-LAST @ reg MASK-BIT and 0 <> if E-CONFLICT throw then
   WRITTEN-LAST @ reg MASK-BIT or WRITTEN-LAST !
   row 1 cells + @ reg REG! ;

: STORE-ROW ( n -- ptr n ) 3 * cells STORES + CELL-VIEW ;

: OVERLAP? ( n n -- bool ) {: a:n b:n :}
   a STORE-ROW @ {: start:n :} start a STORE-ROW 1 cells + @ + {: limit:n :}
   b STORE-ROW @ {: other:n :} other b STORE-ROW 1 cells + @ + {: other-limit:n :}
   start other-limit < other limit < and ;

: STORE-COMMIT ( n -- ) {: idx:n :}
   idx 0 ?do idx i OVERLAP? if E-CONFLICT throw then loop
   idx STORE-ROW {: row:ptr :}
   row 1 cells + @ 1 = if row 2 cells + @ row @ BYTE! else row 2 cells + @ row @ WORD! then ;

\ Lands the packet's writes; two on one register in one cycle are undefined.
: COMMIT ( -- )
   0 WRITTEN-LAST !
   COMMIT-COUNT @ 0 ?do i COMMIT-ROW loop
   STORE-COUNT @ 0 ?do i STORE-COMMIT loop ;


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
   TAKEN @ 0 <> if E-CONFLICT throw then 1 TAKEN !
   BRANCH-PENDING @ 0 <> if UNMODELLED then
   target BRANCH-TARGET ! 1 BRANCH-PENDING ! BRANCH-DELAY BRANCH-LEFT ! ;


\ .L unit forms: three-operand, constant and two-operand, by op field.
: EXECUTE-L ( -- )
   5 7 BITS {: op:n :}
   DST {: dst:n :}
   SRC2 REG@ {: b:n :}
   op 3 = if SRC1 REG@ b + dst RESULT exit then
   op 2 = if SRC1-SIGNED b + dst RESULT exit then
   op 7 = if SRC1 REG@ b - dst RESULT exit then
   op 123 = if SRC1 REG@ b and dst RESULT exit then
   op 127 = if SRC1 REG@ b or dst RESULT exit then
   op 126 = if b dst RESULT exit then
   op 111 = if SRC1 REG@ b xor dst RESULT exit then
   op 83 = if SRC1 REG@ b = if 1 else 0 then dst RESULT exit then
   op 75 = if                                            \ SUBC compares unsigned operands
      SRC1 REG@ {: a:n :}
      a b >= if a b - 1 lshift 1 or else a 1 lshift then dst RESULT exit
   then
   op 71 = if SRC1 REG@ >S32 b >S32 > if 1 else 0 then dst RESULT exit then
   op 70 = if SRC1-SIGNED b >S32 > if 1 else 0 then dst RESULT exit then
   op 79 = if SRC1 REG@ b > if 1 else 0 then dst RESULT exit then
   op 78 = if SRC1-UNSIGNED b > if 1 else 0 then dst RESULT exit then
   op 87 = if SRC1 REG@ >S32 b >S32 < if 1 else 0 then dst RESULT exit then
   op 86 = if SRC1-SIGNED b >S32 < if 1 else 0 then dst RESULT exit then
   op 95 = if SRC1 REG@ b < if 1 else 0 then dst RESULT exit then
   op 94 = if SRC1-UNSIGNED b < if 1 else 0 then dst RESULT exit then
   op 26 = if b $80000000 = if $7FFFFFFF else b >S32 abs then dst RESULT exit then   \ ABS saturates
   op 99 = if b NORM dst RESULT exit then
   UNMODELLED ;


: EXECUTE-S ( -- )
   6 6 BITS {: op:n :}
   DST {: dst:n :}
   SRC2 REG@ {: value:n :}
   op 51 = if value SRC1 REG@ SHL dst RESULT exit then
   op 50 = if value SRC1-UNSIGNED SHL dst RESULT exit then
   op 55 = if value SRC1 REG@ SHR dst RESULT exit then
   op 54 = if value SRC1-UNSIGNED SHR dst RESULT exit then
   op 39 = if value SRC1 REG@ SHRU dst RESULT exit then
   op 38 = if value SRC1-UNSIGNED SHRU dst RESULT exit then
   UNMODELLED ;


: EXECUTE-FIELD ( -- )
   6 2 BITS {: kind:n :}
   DST {: dst:n :}
   SRC2-SAME REG@ {: value:n :}
   13 5 BITS {: csta:n :}
   8 5 BITS {: cstb:n :}
   kind 0 = if value csta SHL cstb SHRU dst RESULT exit then
   kind 1 = if value csta SHL cstb SHR dst RESULT exit then
   cstb csta < if UNMODELLED then
   kind 2 = if value csta cstb FIELD-MASK or dst RESULT exit then
   value csta cstb FIELD-MASK MASK32 xor and dst RESULT ;


: EXECUTE-D ( -- )
   7 6 BITS {: op:n :}
   DST {: dst:n :}
   SRC2-SAME REG@ {: base:n :}
   op 48 = if SRC1 REG@ base + dst RESULT exit then
   op 50 = if SRC1-UNSIGNED base + dst RESULT exit then
   op 56 = if SRC1 REG@ 4 * base + dst RESULT exit then
   op 58 = if SRC1-UNSIGNED 4 * base + dst RESULT exit then
   UNMODELLED ;


\ Loads and stores: mode bits select offset or post-modification and its sign.
\ Loads read memory now and land after the delay; stores land with the packet.
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
      base REG@ {: address:n :} address delta + base RESULT
      address
   else base REG@ delta + then {: address:n :}
   kind 2 = if data address BYTE@ 8 SEXT LOAD-DELAY SCHEDULE exit then
   kind 1 = if data address BYTE@ LOAD-DELAY SCHEDULE exit then
   kind 3 = if data REG@ address 1 STORE exit then
   kind 6 = if data address WORD@ LOAD-DELAY SCHEDULE exit then
   kind 7 = if data REG@ address 4 STORE exit then
   kind 14 = if
      data address WORD@ LOAD-DELAY SCHEDULE data 1+ address 4 + WORD@ LOAD-DELAY SCHEDULE exit
   then
   kind 12 = if data REG@ address 4 STORE data 1+ REG@ address 4 + 4 STORE exit then
   UNMODELLED ;


: EXECUTE-MOVE ( -- )
   7 16 BITS {: value:n :}
   DST {: dst:n :}
   6 1 BITS 0 <> if dst REG@ $FFFF and value 16 lshift or dst RESULT exit then
   value 16 SEXT dst RESULT ;


\ Instruction idx of the packet; a relative branch counts from the fetch packet holding it.
: RUN-ONE ( n -- ) {: idx:n :}
   PACKET idx cells + @ $FFFFFFFE and CURRENT !
   CURRENT @ $FFFE1FFF and 0= if exit then
   CONDITION-TRUE? 0= if exit then
   CURRENT @ $7C and $28 = CURRENT @ $7C and $68 = or if EXECUTE-MOVE exit then
   CURRENT @ $FFF and $362 = if 18 5 BITS 12 1 BITS 1 xor 32 * + REG@ BRANCH exit then
   CURRENT @ $1C and $18 = if EXECUTE-L exit then
   CURRENT @ $3C and $20 = if EXECUTE-S exit then
   CURRENT @ $3C and $08 = if EXECUTE-FIELD exit then
   CURRENT @ $7C and $40 = if EXECUTE-D exit then
   CURRENT @ $7C and $10 = if
      7 21 BITS 21 SEXT 4 * NEXT-WORD @ PACKET-COUNT @ - idx + 4 * $FFFFFFE0 and + BRANCH exit
   then
   CURRENT @ $0C and $04 = if EXECUTE-MEMORY exit then
   UNMODELLED ;

: RUN-PACKET ( -- )
   0 COMMIT-COUNT ! 0 STORE-COUNT ! 0 TAKEN !
   PACKET-COUNT @ 0 ?do i RUN-ONE loop ;


\ ---- packets ----------------------------------------------------------------------------

: FETCH ( -- n )
   NEXT-WORD @ 0 < NEXT-WORD @ PROGRAM-WORDS @ >= or if E-FAULT throw then
   PROGRAM @ {: base:ptr :}
   base NEXT-WORD @ cells + @ >U32 1 NEXT-WORD +! ;

\ The words from NEXT-WORD chained by their p bits.
: GATHER ( -- )
   0 PACKET-COUNT ! 1 MORE !
   begin MORE @ 0 <> while
      PACKET-COUNT @ PACKET-MAX >= if E-CONFLICT throw then
      FETCH {: w:n :}
      w PACKET PACKET-COUNT @ cells + ! 1 PACKET-COUNT +!
      w 1 and MORE !
   repeat ;

\ One cross path carries one register per cycle, though any number of units may read it.
: CLAIM-CROSS ( -- )
   CROSS@ {: reg:n :}
   reg 32 >= if CROSS-1X else CROSS-2X then {: path:ptr :}
   path @ 0 >= path @ reg <> and if E-CONFLICT throw then
   reg path !
   CROSS-READS @ reg MASK-BIT or CROSS-READS ! ;

: CLASSIFY ( -- ) CHECKING @ FACTS ;

\ A word the facts module cannot classify is one the interpreter cannot model.
: CHECK-ONE ( n -- ) {: idx:n :}
   PACKET idx cells + @ CHECKING !
   [: CLASSIFY ;] catch {: code:n :}
   code C6XFACTS:E-DECODE = if CHECKING @ CURRENT ! UNMODELLED then
   code 0 <> if code throw then
   IDLE@ 1 > if IDLE-TICKS @ 0 <> if E-CONFLICT throw then IDLE@ IDLE-TICKS ! then
   IDLE@ 0 <> if exit then
   UNIT@ MASK-BIT UNITS-USED @ and 0 <> if E-CONFLICT throw then
   UNIT@ MASK-BIT UNITS-USED @ or UNITS-USED !
   CROSS@ 0 >= if CLAIM-CROSS then
   MEMORY@ 0 <> if
      DATA-SIDE@ MASK-BIT MEMORY-SIDES @ and 0 <> if E-CONFLICT throw then
      DATA-SIDE@ MASK-BIT MEMORY-SIDES @ or MEMORY-SIDES !
   then ;

\ The packet's resource use against SPRUGH7 3.8.1, 3.8.4, 3.8.6 and 3.8.11.
: CHECK ( -- )
   0 UNITS-USED ! -1 CROSS-1X ! -1 CROSS-2X ! 0 MEMORY-SIDES ! 0 IDLE-TICKS ! 0 CROSS-READS !
   PACKET-COUNT @ 0 ?do i CHECK-ONE loop ;

\ One cycle passes: due loads land, and a pending branch spends a delay slot.
: TICK ( -- )
   SETTLE 1 CYCLES +!
   WRITTEN-LAST @ RECENT ! 0 WRITTEN-LAST !
   BRANCH-PENDING @ 0 <> if -1 BRANCH-LEFT +! then ;

\ Reading through a cross path a register a non-load wrote last cycle costs a cycle (3.8.5).
: STALL ( -- )
   CROSS-READS @ RECENT @ and 0= if exit then
   SETTLE 1 CYCLES +! ;

\ A branch whose delay slots have run: to the return address, draining the
\ loads still in flight, or to a word of the program.
: BRANCH-DUE ( -- )
   BRANCH-PENDING @ 0= if exit then
   BRANCH-LEFT @ 0 > if exit then
   0 BRANCH-PENDING ! 1 JUMPED !
   BRANCH-TARGET @ RETURN-ADDRESS = if
      begin PENDING-COUNT @ 0 > while TICK repeat
      1 DONE ! exit
   then
   BRANCH-TARGET @ 4 mod 0 <> if E-FAULT throw then
   BRANCH-TARGET @ 4 / NEXT-WORD ! ;

: IDLE-ONE ( -- ) JUMPED @ 0 <> DONE @ 0 <> or if exit then TICK BRANCH-DUE ;

\ Executes the packet at NEXT-WORD in one cycle, plus the extra cycles of a
\ multicycle NOP in it, which a branch landing first cuts short.
: STEP ( -- )
   GATHER CHECK TICK STALL RUN-PACKET COMMIT
   0 JUMPED ! BRANCH-DUE
   IDLE-TICKS @ 1 > if IDLE-TICKS @ 1- 0 ?do IDLE-ONE loop then ;

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
: CALL ( ptr n n -- n ) {: words:ptr count:n :}
   count 0 <= if E-OPERAND throw then
   words PROGRAM ! count PROGRAM-WORDS ! 0 NEXT-WORD ! 0 CYCLES ! 0 BRANCH-PENDING ! 0 PENDING-COUNT !
   0 WRITTEN-LAST ! 0 RECENT ! 0 LANDED !
   RETURN-ADDRESS 3 B! 0 DONE !
   begin DONE @ 0= while
      STEP
      CYCLES @ BUDGET @ > if E-LIMIT throw then
   repeat
   CYCLES @ ;

;package

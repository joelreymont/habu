\ emit-x64.f - the x86-64 bytes of one accepted routine, laid out in BYTES and
\ placed at the slot the driver names.
\
\ IT IS THE TWIN OF src/compiler/native/emit.f AND NOT A PARAMETERISATION OF IT.
\ docs/x86-64.md "The emitter's sink" decided that before any emitter code: that
\ file is a FIXED-WIDTH word sink - four bytes an instruction, a capacity counted
\ in instructions, a source map keyed by instruction ordinal - and an x86-64
\ instruction is one to fifteen bytes. Everything this file reads back is
\ therefore counted in BYTES where the ARM64 emitter counts instructions.
\
\ THE SINK IS THIS PASS'S OWN. Two lib/byte-buffer.f BUFs: the emission, which is
\ what BYTES and SIZE answer, and a scratch one nothing outside a measurement
\ ever sees. EVERY LENGTH HERE IS MEASURED AND NONE IS WRITTEN DOWN: an
\ operation's size is what encoding it into the scratch sink leaves there, which
\ is the only statement of a size that cannot drift from what src/arch/x86-64/
\ asm.f really writes.
\
\ TWO PASSES, AND ONE ORDER. MEASURE walks the blocks in the chosen order and
\ files where each one starts; WRITE-ALL walks the same order and writes, and
\ arriving at a block anywhere but its measured start is E-X64EMIT-LAYOUT rather
\ than a silently different routine. A branch is written in the WIDE form always
\ (rel32), so no displacement can change a length between the two passes and no
\ third pass is needed; shortening one that fits a byte is a relaxation this
\ emitter deliberately has not got (docs/x86-64.md "The emitter's sink").
\
\ A DISPLACEMENT COUNTS FROM THE END OF ITS INSTRUCTION, which is the x86 rule
\ and NOT ARM64's - emit.f DELTA counts from the instruction's own address. So
\ every branch here asks the scratch sink how long the instruction it is about to
\ write is, and subtracts that too.
\
\ REGISTERS COME THROUGH ONE DOOR. A64RAV:REG@ is the only way a register number
\ reaches an instruction here, exactly as in emit.f, and the slot and byte
\ offsets come off the module's own attributes. A TWO-ADDRESS FORM WRITES THE
\ RESULT'S REGISTER: the dialect ties the result to operand zero
\ (src/compiler/native/x64ir.f DEF-BINARY) and the allocator refuses a tie it
\ cannot satisfy, so the tie is the accepted allocation's guarantee and not a
\ second rule to re-derive here.
\
\ A COPY IS WRITTEN AS IT STANDS. `x64.mov` whose two ends the allocator
\ coalesced onto one register is three bytes of `mov rax, rax`; eliding it is a
\ peephole this file does not have, and what the pinned bytes in
\ test/compiler/x64-emit.f measure is a faithful image of the module.
\
\ WHAT IS STILL REFUSED BY NAME, each with E-X64EMIT-FORM: the variable shifts
\ `shl`/`shr` and the divide `idiv` - all three name a register the machine
\ chose and the divide carries a branch to `x64.throw-entry` besides - the
\ negate, the two selects `cmpsel` and `selz`, the four frame forms `reserve`,
\ `release`, `store` and `load`, the trap, and `codeaddr`. The float forms are
\ not declared by the dialect at all. Publication into a code region is not here
\ either, and on this host it cannot be: src/compiler/native/publish.f names
\ A64EMIT at thirteen sites and the engine's own callmap and addrmap record
\ ARM64 shapes. An x86-64 emission is consumed by the cross-build image writer.

require lib/prelude.f
require lib/errors.f
require lib/num-types.f
require lib/byte-buffer.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/build.f
require src/compiler/native/x64ir.f
require src/compiler/native/frozen.f
require src/compiler/native/regalloc.f
require src/compiler/native/regalloc-verify.f
require src/arch/x86-64/asm.f
require src/arch/x86-64/machine.f

package X64EMIT
using X64ASM
using NFROZEN
private

\ ---- the byte sinks ----------------------------------------------------------
\ The lengths BUF answers are a role; the layout arithmetic, the block starts and
\ the site offsets are raw cells, so the projection is taken once here the way
\ lib/byte-buffer.f takes its own.
CAST: BLEN>N ( NUM:byte-len -- n )

: N>BLEN ( n -- NUM:byte-len )
   NUM:BYTE-LEN
   MATCH NUM:numeric-result
      ok OF ENDOF                                negative OF E-BUF-BOUNDS throw ENDOF
      zero OF E-BUF-BOUNDS throw ENDOF           overflow OF E-BUF-BOUNDS throw ENDOF
      underflow OF E-BUF-BOUNDS throw ENDOF      bad-alignment OF E-BUF-BOUNDS throw ENDOF
      misaligned OF E-BUF-BOUNDS throw ENDOF
   ;MATCH ;

\ Both grow by themselves, so these are a first extent and not a ceiling.
256 constant SINK-CAP0                  \ bytes the emission starts with
64 constant SCRATCH-CAP                 \ and the scratch, which holds ONE operation

create EM-SINK BUF:HDR-BYTES allot      \ the emission this pass hands back
create SC-SINK BUF:HDR-BYTES allot      \ and the one every measurement writes into

here CELL 1- and CELL swap - CELL 1- and allot
variable SINK-MODE                      \ nonzero once both sinks are mapped

0 constant BOUND-NO
1 constant BOUND-YES
variable BND-MODE

0 constant ST-EMPTY
1 constant ST-SEALED
variable ST

0 constant PLACE-NO
1 constant PLACE-YES
variable PLACE-MODE
variable PLACE-AT-N

variable MEAS                           \ nonzero while an operation is being measured
variable MCUR                           \ the measuring pass's byte cursor
variable N-BLK                          \ blocks in the function being laid out
variable N-LAID                         \ ...and how many of them the order holds
variable B-BASE                         \ where this function's blocks start in the module
variable N-FUNS
variable N-SITES
variable EM-LAST                        \ the form of the last operation written
variable CH-AT                          \ the pass-through chase's cursor

variable SCRATCH-BLOCKS
variable SCRATCH-FUNS
variable SCRATCH-OPS
: BMAX ( -- n ) SCRATCH-BLOCKS @ ;
: FMAX ( -- n ) SCRATCH-FUNS @ ;
: OMAX ( -- n ) SCRATCH-OPS @ ;

BOUND-NO BND-MODE !
ST-EMPTY ST !
PLACE-NO PLACE-MODE !

DYNAMIC-BUFFER B-START-BUF n            \ where a block starts, in bytes
: B-START ( -- ptr n ) 0 B-START-BUF ;
DYNAMIC-BUFFER B-ORDER-BUF n            \ position -> block ordinal
: B-ORDER ( -- ptr n ) 0 B-ORDER-BUF ;
DYNAMIC-BUFFER B-PLACE-BUF n            \ block ordinal -> position
: B-PLACE ( -- ptr n ) 0 B-PLACE-BUF ;
DYNAMIC-BUFFER B-GOTO-BUF n             \ where a branch to this block really goes
: B-GOTO ( -- ptr n ) 0 B-GOTO-BUF ;
DYNAMIC-BUFFER B-KEEP-BUF n             \ whether anything still reaches it
: B-KEEP ( -- ptr n ) 0 B-KEEP-BUF ;
DYNAMIC-BUFFER F-START-BUF n            \ where a function starts, in bytes
: F-START ( -- ptr n ) 0 F-START-BUF ;
DYNAMIC-BUFFER SITES-BUF n              \ byte offset of a relocatable literal
: SITES ( -- ptr n ) 0 SITES-BUF ;
DYNAMIC-BUFFER SKIND-BUF n              \ and which kind of address it carries
: SKIND ( -- ptr n ) 0 SKIND-BUF ;

: SINK-READY ( -- )
   SINK-MODE @ 0<> if exit then
   EM-SINK SINK-CAP0 N>BLEN BUF:INIT
   SC-SINK SCRATCH-CAP N>BLEN BUF:INIT
   1 SINK-MODE ! ;

: EM-LEN ( -- n )  EM-SINK BUF:LEN@ BLEN>N ;
: SC-LEN ( -- n )  SC-SINK BUF:LEN@ BLEN>N ;
: SC-CLEAR ( -- )  SC-SINK BUF:CLEAR ;

\ Where the next byte of the EMISSION goes. Asked only while the writer is
\ filling it: a measurement has no cursor, and says so by writing zero into every
\ displacement field (REL-TO below).
: CUR ( -- n )  EM-LEN ;

: SCRATCH-SIZES! ( -- )
   NFROZEN:TOTAL-BLOCKS 1 max SCRATCH-BLOCKS !
   NFROZEN:TOTAL-FUNS 1 max SCRATCH-FUNS !
   NFROZEN:TOTAL-OPS 1 max SCRATCH-OPS ! ;

\ One operation opens at most one site, so the module's own operation count is
\ the ceiling and no number is chosen here.
: SITE-CEIL ( -- n ) OMAX ;

: RESERVE-SCRATCH ( -- )
   SCRATCH-SIZES!
   BMAX B-START-BUF-RESERVE
   BMAX B-ORDER-BUF-RESERVE
   BMAX B-PLACE-BUF-RESERVE
   BMAX B-GOTO-BUF-RESERVE
   BMAX B-KEEP-BUF-RESERVE
   FMAX F-START-BUF-RESERVE
   SITE-CEIL SITES-BUF-RESERVE
   SITE-CEIL SKIND-BUF-RESERVE ;

\ ---- the bound dialect -------------------------------------------------------
\ A module's symbols are its own ordinals, so the identities are taken once from
\ the module about to be written and never spelled again.
1 TYPED-BUFFER BND-MOD IR-ID:ir-module-id
X64IR:OPCODES TYPED-BUFFER BND-OP IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-IMM IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-ADDR IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-SHIFT IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-COND IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-DSLOT IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-DBYTES IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-DBACK IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-ENTRY IR-ID:ir-symbol-id

\ ---- the dialect's operation family ------------------------------------------
\ An opcode this dialect does not own has no encoding here and is refused rather
\ than guessed.
: OPCODE-SLOT ( IR-ID:ir-symbol-id -- n )
   {: sym:IR-ID:ir-symbol-id :}
   -1
   X64IR:OPCODES 0 ?do
      sym i BND-OP @ SAME-SYM? if drop i leave then
   loop
   dup 0 < if E-X64EMIT-OPCODE throw then ;

: SLOT-AT ( IR-ID:ir-op-id -- n )
   OPCODE-AT OPCODE-SLOT ;

\ The forms this emitter has to recognise before it writes anything: the ones
\ that end control, and the one that ends the routine. They are named here by
\ ordinal because the shape check and the block order read ordinals.
X64IR-OPCODE:RET      X64IR:ORD constant O-RET
X64IR-OPCODE:BR       X64IR:ORD constant O-BR
X64IR-OPCODE:BRZ      X64IR:ORD constant O-BRZ
X64IR-OPCODE:CMPBR    X64IR:ORD constant O-CMPBR
X64IR-OPCODE:CMPBRI   X64IR:ORD constant O-CMPBRI
X64IR-OPCODE:TAILCALL X64IR:ORD constant O-TAILCALL
X64IR-OPCODE:TRAP     X64IR:ORD constant O-TRAP

\ ---- the registers, through the one door that answers ------------------------
: REG-OF ( IR-ID:ir-value-id -- n )
   IR-ID:VALUE-LOCAL A64RAV:REG@ ;

: RES-R64 ( IR-ID:ir-op-id n -- r64 )
   RESULT-AT REG-OF >R64 ;

: OPD-R64 ( IR-ID:ir-op-id n -- r64 )
   OPERAND-AT REG-OF >R64 ;

\ The same register named as one BYTE of itself, which is the file `setcc` and
\ the byte store write. src/arch/x86-64/asm.f spells 4..7 as spl/bpl/sil/dil and
\ never as the legacy high halves, so this is the whole low byte of the register
\ the allocation named.
: RES-R8 ( IR-ID:ir-op-id n -- r8 )
   RESULT-AT REG-OF >R8 ;

: OPD-R8 ( IR-ID:ir-op-id n -- r8 )
   OPERAND-AT REG-OF >R8 ;

\ ---- the attributes an operation carries -------------------------------------
: ATTR-SLOT ( IR-ID:ir-op-id IR-ID:ir-symbol-id -- n )
   {: id:IR-ID:ir-op-id want:IR-ID:ir-symbol-id :}
   -1
   id ATTRS-OF {: n:n :}
   n 0 ?do
      id i ATTR-KEY-AT want SAME-SYM? if drop i leave then
   loop
   dup 0 < if E-X64EMIT-ATTR throw then ;

: ATTR-INT ( IR-ID:ir-op-id IR-ID:ir-symbol-id -- n )
   {: id:IR-ID:ir-op-id want:IR-ID:ir-symbol-id :}
   id want ATTR-SLOT {: k:n :}
   id k ATTR-INT-AT ;

: IMM-OF ( IR-ID:ir-op-id -- n )     0 BND-IMM @ ATTR-INT ;
: ADDR-OF ( IR-ID:ir-op-id -- n )    0 BND-ADDR @ ATTR-INT ;
: SHIFT-OF ( IR-ID:ir-op-id -- n )   0 BND-SHIFT @ ATTR-INT ;
: DSLOT-OF ( IR-ID:ir-op-id -- n )   0 BND-DSLOT @ ATTR-INT ;
: DBYTES-OF ( IR-ID:ir-op-id -- n )  0 BND-DBYTES @ ATTR-INT ;
: DBACK-OF ( IR-ID:ir-op-id -- n )   0 BND-DBACK @ ATTR-INT ;
: ENTRY-OF ( IR-ID:ir-op-id -- n )   0 BND-ENTRY @ ATTR-INT ;

\ The dialect's condition field IS the assembler's four-bit tttn: x64ir.f takes
\ every code from X64ASM:C-L, C-LE, C-G, C-GE, C-E and C-NE rather than writing
\ the numbers again, so the field crosses into the encoder unchanged and the
\ encoder screens it.
: COND-OF ( IR-ID:ir-op-id -- condition )
   0 BND-COND @ ATTR-INT >CONDITION ;

\ ---- the data-stack pointer --------------------------------------------------
\ Asked for by name from the machine description, the way emit.f asks
\ A64M:DSTACK-GPR, so no pass writes 12.
: DSTACK ( -- r64 )
   X64M:DSTACK-GPR >R64 ;

\ ---- where a block was laid --------------------------------------------------
: BLK-ORD-CK ( n -- n )
   dup 0 < over N-BLK @ >= or if E-X64EMIT-BOUND throw then ;

: AT-POS ( n -- n )
   BLK-ORD-CK cells B-ORDER + @ ;

: POS-OF ( n -- n )
   BLK-ORD-CK cells B-PLACE + @ ;

: LAID? ( n -- bool )
   BLK-ORD-CK cells B-PLACE + @ 0 >= ;

: LAY ( n n -- )
   {: b:n p:n :}
   b BLK-ORD-CK {: bb:n :}
   p BLK-ORD-CK {: pp:n :}
   bb  pp cells B-ORDER + !
   pp  bb cells B-PLACE + ! ;

\ Read after GOTO! has run, so it holds the far end of however long a chain of
\ pass-through blocks stood in the way.
: GOTO-OF ( n -- n )
   BLK-ORD-CK cells B-GOTO + @ ;

: KEPT? ( n -- bool )
   BLK-ORD-CK cells B-KEEP + @ 0<> ;

: START-AT ( n -- n )
   BLK-ORD-CK
   dup LAID? 0= if E-X64EMIT-BOUND throw then
   cells B-START + @ ;

\ A successor carries a block's ordinal in the MODULE and these tables are keyed
\ by its ordinal in the FUNCTION, so the function's base comes off it here.
: SUCC-BLOCK ( IR-ID:ir-op-id n -- n )
   SUCC-AT IR-ID:BLOCK-LOCAL  B-BASE @ -  BLK-ORD-CK ;

\ Which successor control REACHES BY FALLING INTO, where the order puts it next.
: TAIL-SUCC ( n -- n )
   {: k:n :}
   k O-BR = if 0 exit then
   k O-BRZ = if 1 exit then
   k O-CMPBR = if 1 exit then
   k O-CMPBRI = if 1 exit then
   -1 ;

\ Asked in POSITIONS and answered about ORDINALS, so nothing here depends on
\ where any block starts - which is what lets the layout ask it.
: FALL-THRU? ( IR-ID:ir-op-id n -- bool )
   {: id:IR-ID:ir-op-id home:n :}
   id SLOT-AT TAIL-SUCC {: s:n :}
   s 0 < if false exit then
   id s SUCC-BLOCK GOTO-OF POS-OF  home POS-OF 1+ = ;

\ ---- the reach of a displacement ---------------------------------------------
\ Both ends of a rel32 are inside one image and the encoder REFUSES a field that
\ does not hold its number rather than masking it, so this check is here to name
\ the emitter's own boundary before the assembler names its operand's.
: FIT ( n -- rel )
   {: d:n :}
   d X64IR:IMM-LIMIT negate <  d X64IR:IMM-LIMIT >=  or
   if E-X64EMIT-REACH throw then
   d >REL ;

\ THE DISPLACEMENT IS FROM THE END, so the instruction's own length is part of
\ it. Reached only from the writing pass: a measurement writes zero (below).
: REL-TO ( n n -- rel )
   {: t:n l:n :}
   t  CUR l +  -  FIT ;

\ How long a branch is, measured the way every other length here is. A probe
\ writes into the scratch sink, so it may only run while the WRITER is filling
\ the emission sink - which is why every caller tests MEAS first.
: JMP-SIZE ( -- n )
   SC-CLEAR  0 >REL SC-SINK ENC-JMP-REL32  SC-LEN ;

: CALL-SIZE ( -- n )
   SC-CLEAR  0 >REL SC-SINK ENC-CALL-REL32  SC-LEN ;

: JCC-SIZE ( condition -- n )
   {: c:condition :}
   SC-CLEAR  c 0 >REL SC-SINK ENC-JCC-REL32  SC-LEN ;

\ A measurement has no cursor and no laid-out target, and the field's WIDTH does
\ not depend on the number in it, so it is written as zero and the real one is
\ written by the pass that knows both ends.
: PUT-JMP ( n ptr a -- )
   {: t:n s:ptr :}
   MEAS @ 0<> if 0 >REL s ENC-JMP-REL32 exit then
   t JMP-SIZE REL-TO s ENC-JMP-REL32 ;

: PUT-CALL-TO ( n ptr a -- )
   {: t:n s:ptr :}
   MEAS @ 0<> if 0 >REL s ENC-CALL-REL32 exit then
   t CALL-SIZE REL-TO s ENC-CALL-REL32 ;

: PUT-JCC ( condition n ptr a -- )
   {: c:condition t:n s:ptr :}
   MEAS @ 0<> if c 0 >REL s ENC-JCC-REL32 exit then
   c  t  c JCC-SIZE REL-TO  s ENC-JCC-REL32 ;

\ Where a block was laid, or zero while measuring: no block has a start yet and
\ the number is not in the bytes anyway.
: BLOCK-TARGET ( n -- n )
   MEAS @ 0<> if drop 0 exit then
   START-AT ;

\ ---- where this emission's relocatable literals are --------------------------
\ ONE SITE PER LITERAL, because x64ir.f ADDR-LANES is 1: the whole address is the
\ imm64 of one `mov r64, imm64` and the writer patches at
\ X64ASM:MOV-RI64-IMM-OFF from the offset filed here. ARM64's chain check has no
\ counterpart: there is no run of lanes to hold together.
: SITE+ ( n n -- )
   {: off:n kind:n :}
   N-SITES @ SITE-CEIL >= if E-X64EMIT-BOUND throw then
   off  N-SITES @ cells SITES + !
   kind N-SITES @ cells SKIND + !
   N-SITES @ 1+ N-SITES ! ;

\ ---- one operation, written --------------------------------------------------
: PUT-MOVI ( IR-ID:ir-op-id ptr a -- )
   {: id:IR-ID:ir-op-id s:ptr :}
   id ADDR-OF {: kind:n :}
   MEAS @ 0=  kind X64IR:ADDR-NONE <>  and if CUR kind SITE+ then
   id 0 RES-R64  id IMM-OF >IMM64  s ENC-MOV-RI64 ;

: PUT-MOV ( IR-ID:ir-op-id ptr a -- )
   {: id:IR-ID:ir-op-id s:ptr :}
   id 0 RES-R64  id 0 OPD-R64  s ENC-MOV-RR ;

: PUT-ADD ( IR-ID:ir-op-id ptr a -- )
   {: id:IR-ID:ir-op-id s:ptr :}
   id 0 RES-R64  id 1 OPD-R64  s ENC-ADD-RR ;

: PUT-SUB ( IR-ID:ir-op-id ptr a -- )
   {: id:IR-ID:ir-op-id s:ptr :}
   id 0 RES-R64  id 1 OPD-R64  s ENC-SUB-RR ;

: PUT-AND ( IR-ID:ir-op-id ptr a -- )
   {: id:IR-ID:ir-op-id s:ptr :}
   id 0 RES-R64  id 1 OPD-R64  s ENC-AND-RR ;

: PUT-OR ( IR-ID:ir-op-id ptr a -- )
   {: id:IR-ID:ir-op-id s:ptr :}
   id 0 RES-R64  id 1 OPD-R64  s ENC-OR-RR ;

: PUT-XOR ( IR-ID:ir-op-id ptr a -- )
   {: id:IR-ID:ir-op-id s:ptr :}
   id 0 RES-R64  id 1 OPD-R64  s ENC-XOR-RR ;

: PUT-IMUL ( IR-ID:ir-op-id ptr a -- )
   {: id:IR-ID:ir-op-id s:ptr :}
   id 0 RES-R64  id 1 OPD-R64  s ENC-IMUL-RR ;

\ ONE ENCODING PER OPERATION, which is src/arch/x86-64/asm.f's own rule: the
\ dialect's immediate is the signed thirty-two bits the machine sign-extends, so
\ the imm32 form is written whatever the value's magnitude. Shortening the ones
\ that fit an imm8 is a size optimisation and would make an instruction's length
\ depend on its operand.
: PUT-ADDI ( IR-ID:ir-op-id ptr a -- )
   {: id:IR-ID:ir-op-id s:ptr :}
   id 0 RES-R64  id IMM-OF >IMM32  s ENC-ADD-RI32 ;

: PUT-SUBI ( IR-ID:ir-op-id ptr a -- )
   {: id:IR-ID:ir-op-id s:ptr :}
   id 0 RES-R64  id IMM-OF >IMM32  s ENC-SUB-RI32 ;

: PUT-ANDI ( IR-ID:ir-op-id ptr a -- )
   {: id:IR-ID:ir-op-id s:ptr :}
   id 0 RES-R64  id IMM-OF >IMM32  s ENC-AND-RI32 ;

: PUT-ORI ( IR-ID:ir-op-id ptr a -- )
   {: id:IR-ID:ir-op-id s:ptr :}
   id 0 RES-R64  id IMM-OF >IMM32  s ENC-OR-RI32 ;

: PUT-XORI ( IR-ID:ir-op-id ptr a -- )
   {: id:IR-ID:ir-op-id s:ptr :}
   id 0 RES-R64  id IMM-OF >IMM32  s ENC-XOR-RI32 ;

: PUT-SHLI ( IR-ID:ir-op-id ptr a -- )
   {: id:IR-ID:ir-op-id s:ptr :}
   id 0 RES-R64  id SHIFT-OF >IMM8  s ENC-SHL-RI8 ;

: PUT-SHRI ( IR-ID:ir-op-id ptr a -- )
   {: id:IR-ID:ir-op-id s:ptr :}
   id 0 RES-R64  id SHIFT-OF >IMM8  s ENC-SHR-RI8 ;

: PUT-NOT ( IR-ID:ir-op-id ptr a -- )
   {: id:IR-ID:ir-op-id s:ptr :}
   id 0 RES-R64  s ENC-NOT ;

\ THREE INSTRUCTIONS FOR ONE OPERATION, because the flags between them are a
\ single architectural resource no SSA value stands for: compare, set the
\ result's low byte on the condition, widen that byte into the whole register.
: PUT-SETCC ( IR-ID:ir-op-id ptr a -- )
   {: id:IR-ID:ir-op-id s:ptr :}
   id COND-OF  id 0 RES-R8  s ENC-SETCC
   id 0 RES-R64  id 0 RES-R8  s ENC-MOVZX-8-RR ;

: PUT-CMPSET ( IR-ID:ir-op-id ptr a -- )
   {: id:IR-ID:ir-op-id s:ptr :}
   id 0 OPD-R64  id 1 OPD-R64  s ENC-CMP-RR
   id s PUT-SETCC ;

: PUT-CMPSETI ( IR-ID:ir-op-id ptr a -- )
   {: id:IR-ID:ir-op-id s:ptr :}
   id 0 OPD-R64  id IMM-OF >IMM32  s ENC-CMP-RI32
   id s PUT-SETCC ;

\ ---- the branches ------------------------------------------------------------
\ The arguments are already in the destination's registers by the allocation's
\ own decision, so they reach no encoder and the whole operation is the jump -
\ and where the destination is the very next block in the order, not even that.
: PUT-BR ( IR-ID:ir-op-id n ptr a -- )
   {: id:IR-ID:ir-op-id home:n s:ptr :}
   id home FALL-THRU? if exit then
   id 0 SUCC-BLOCK GOTO-OF BLOCK-TARGET  s PUT-JMP ;

\ `test rv, rv` and a jump on the zero flag: this machine has no branch on a
\ register, but it has a one-instruction way to set the flag from one.
: PUT-BRZ ( IR-ID:ir-op-id n ptr a -- )
   {: id:IR-ID:ir-op-id home:n s:ptr :}
   id 0 OPD-R64 {: r:r64 :}
   r r s ENC-TEST-RR
   C-E  id 0 SUCC-BLOCK GOTO-OF BLOCK-TARGET  s PUT-JCC
   id home FALL-THRU? if exit then
   id 1 SUCC-BLOCK GOTO-OF BLOCK-TARGET  s PUT-JMP ;

\ The comparison writes only the flags and the branch beside it reads them
\ there, so no register is written. The FIRST successor is the condition-holds
\ one, which is the order x64ir.f DEF-CMPBR states and a64.cmpbr measured.
: PUT-CMPBR ( IR-ID:ir-op-id n ptr a -- )
   {: id:IR-ID:ir-op-id home:n s:ptr :}
   id 0 OPD-R64  id 1 OPD-R64  s ENC-CMP-RR
   id COND-OF  id 0 SUCC-BLOCK GOTO-OF BLOCK-TARGET  s PUT-JCC
   id home FALL-THRU? if exit then
   id 1 SUCC-BLOCK GOTO-OF BLOCK-TARGET  s PUT-JMP ;

: PUT-CMPBRI ( IR-ID:ir-op-id n ptr a -- )
   {: id:IR-ID:ir-op-id home:n s:ptr :}
   id 0 OPD-R64  id IMM-OF >IMM32  s ENC-CMP-RI32
   id COND-OF  id 0 SUCC-BLOCK GOTO-OF BLOCK-TARGET  s PUT-JCC
   id home FALL-THRU? if exit then
   id 1 SUCC-BLOCK GOTO-OF BLOCK-TARGET  s PUT-JMP ;

\ ---- the data-stack crossings ------------------------------------------------
\ NO INSTRUCTION AT ALL when the distance is zero, which is emit.f's rule for
\ the same move. x86-64 has no write-back addressing, so the move is its own
\ instruction and never rides on a transfer.
: PUT-DMOVE ( n ptr a -- )
   {: d:n s:ptr :}
   d 0= if exit then
   d 0 > if
      DSTACK  d >IMM32  s ENC-ADD-RI32
      exit
   then
   DSTACK  d negate >IMM32  s ENC-SUB-RI32 ;

: PUT-DTAKE ( IR-ID:ir-op-id ptr a -- )
   {: id:IR-ID:ir-op-id s:ptr :}
   id DBYTES-OF negate  s PUT-DMOVE ;

: PUT-DPUBLISH ( IR-ID:ir-op-id ptr a -- )
   {: id:IR-ID:ir-op-id s:ptr :}
   id DBYTES-OF  s PUT-DMOVE ;

\ The offset is SIGNED and counted in bytes from where the body's pointer
\ stands, which is what src/arch/x86-64/machine.f says a displacement of this
\ machine is.
: PUT-DLOAD ( IR-ID:ir-op-id ptr a -- )
   {: id:IR-ID:ir-op-id s:ptr :}
   id 0 RES-R64  DSTACK id DSLOT-OF MEM-OFF  s ENC-MOV-RM ;

: PUT-DSTORE ( IR-ID:ir-op-id ptr a -- )
   {: id:IR-ID:ir-op-id s:ptr :}
   id 0 OPD-R64  DSTACK id DSLOT-OF MEM-OFF  s ENC-MOV-MR ;

\ ---- the addressed forms -----------------------------------------------------
\ The dialect has no offset attribute on these: the address is the whole operand
\ and the form encodes at displacement zero.
: PUT-ALOAD ( IR-ID:ir-op-id ptr a -- )
   {: id:IR-ID:ir-op-id s:ptr :}
   id 0 RES-R64  id 0 OPD-R64 MEM-AT  s ENC-MOV-RM ;

: PUT-ASTORE ( IR-ID:ir-op-id ptr a -- )
   {: id:IR-ID:ir-op-id s:ptr :}
   id 0 OPD-R64  id 1 OPD-R64 MEM-AT  s ENC-MOV-MR ;

\ THE WIDTH IS THE FORM. `movzx r64, byte [r]` is what `c@` leaves and
\ `mov byte [r], r8` writes only the low byte, so a cell-wide encoder here would
\ move seven bytes the program never named.
: PUT-ABLOAD ( IR-ID:ir-op-id ptr a -- )
   {: id:IR-ID:ir-op-id s:ptr :}
   id 0 RES-R64  id 0 OPD-R64 MEM-AT  s ENC-MOVZX-8-RM ;

: PUT-ABSTORE ( IR-ID:ir-op-id ptr a -- )
   {: id:IR-ID:ir-op-id s:ptr :}
   id 0 OPD-R8  id 1 OPD-R64 MEM-AT  s ENC-MOV8-MR ;

\ ---- the calls ---------------------------------------------------------------
\ A self-call is RECURSE, which names the DEFINITION and not the body the token
\ stands in, so it goes to function zero of this emission wherever that was
\ staged. The pointer is moved out and back around it, because a call hands its
\ arguments over through the data stack.
0 constant SELF-FUN

: FUN-START ( n -- n )
   {: k:n :}
   k 0 < k N-FUNS @ >= or if E-X64EMIT-BOUND throw then
   k cells F-START + @ ;

: PUT-CALL ( IR-ID:ir-op-id ptr a -- )
   {: id:IR-ID:ir-op-id s:ptr :}
   id DBYTES-OF s PUT-DMOVE
   SELF-FUN FUN-START s PUT-CALL-TO
   id DBACK-OF negate s PUT-DMOVE ;

\ ANOTHER WORD'S ENTRY IS AN ABSOLUTE ADDRESS, so the distance is measured from
\ where this emission was placed: the instruction's own address is the placement
\ plus its offset in the emission, and the displacement is from the end of it.
: ENTRY-TARGET ( IR-ID:ir-op-id -- n )
   {: id:IR-ID:ir-op-id :}
   PLACE-MODE @ PLACE-YES <> if E-X64EMIT-STATE throw then
   id ENTRY-OF PLACE-AT-N @ - ;

: PUT-WORDCALL ( IR-ID:ir-op-id ptr a -- )
   {: id:IR-ID:ir-op-id s:ptr :}
   id DBYTES-OF s PUT-DMOVE
   id ENTRY-TARGET s PUT-CALL-TO
   id DBACK-OF negate s PUT-DMOVE ;

\ ONE instruction and never more: the selector only builds it where the pointer
\ already stands at the callee's entry base, and the callee returns to THIS
\ routine's caller.
: PUT-TAILCALL ( IR-ID:ir-op-id ptr a -- )
   {: id:IR-ID:ir-op-id s:ptr :}
   id ENTRY-TARGET s PUT-JMP ;

\ ---- the dispatch ------------------------------------------------------------
\ Every opcode of the dialect is named, so a form added to the vocabulary is a
\ decision taken HERE rather than a silent fall-through. The refusing arms are
\ the forms this emitter does not render: each needs a register the machine
\ names, a prologue or a lowering, and none of those is here.
: PUT-OP ( IR-ID:ir-op-id n ptr a -- )
   {: id:IR-ID:ir-op-id home:n s:ptr :}
   id SLOT-AT X64IR:NTH
   MATCH X64IR:opcode
      movi     OF id s PUT-MOVI ENDOF
      mov      OF id s PUT-MOV ENDOF
      add      OF id s PUT-ADD ENDOF
      sub      OF id s PUT-SUB ENDOF
      and      OF id s PUT-AND ENDOF
      or       OF id s PUT-OR ENDOF
      xor      OF id s PUT-XOR ENDOF
      imul     OF id s PUT-IMUL ENDOF
      addi     OF id s PUT-ADDI ENDOF
      subi     OF id s PUT-SUBI ENDOF
      andi     OF id s PUT-ANDI ENDOF
      ori      OF id s PUT-ORI ENDOF
      xori     OF id s PUT-XORI ENDOF
      shli     OF id s PUT-SHLI ENDOF
      shri     OF id s PUT-SHRI ENDOF
      shl      OF E-X64EMIT-FORM throw ENDOF
      shr      OF E-X64EMIT-FORM throw ENDOF
      neg      OF E-X64EMIT-FORM throw ENDOF
      not      OF id s PUT-NOT ENDOF
      idiv     OF E-X64EMIT-FORM throw ENDOF
      cmpset   OF id s PUT-CMPSET ENDOF
      cmpseti  OF id s PUT-CMPSETI ENDOF
      cmpsel   OF E-X64EMIT-FORM throw ENDOF
      selz     OF E-X64EMIT-FORM throw ENDOF
      br       OF id home s PUT-BR ENDOF
      brz      OF id home s PUT-BRZ ENDOF
      cmpbr    OF id home s PUT-CMPBR ENDOF
      cmpbri   OF id home s PUT-CMPBRI ENDOF
      ret      OF s ENC-RET ENDOF
      reserve  OF E-X64EMIT-FORM throw ENDOF
      release  OF E-X64EMIT-FORM throw ENDOF
      store    OF E-X64EMIT-FORM throw ENDOF
      load     OF E-X64EMIT-FORM throw ENDOF
      dtake    OF id s PUT-DTAKE ENDOF
      dload    OF id s PUT-DLOAD ENDOF
      dstore   OF id s PUT-DSTORE ENDOF
      dpublish OF id s PUT-DPUBLISH ENDOF
      aload    OF id s PUT-ALOAD ENDOF
      astore   OF id s PUT-ASTORE ENDOF
      abload   OF id s PUT-ABLOAD ENDOF
      abstore  OF id s PUT-ABSTORE ENDOF
      call     OF id s PUT-CALL ENDOF
      wordcall OF id s PUT-WORDCALL ENDOF
      tailcall OF id s PUT-TAILCALL ENDOF
      trap     OF E-X64EMIT-FORM throw ENDOF
      codeaddr OF E-X64EMIT-FORM throw ENDOF
   ;MATCH ;

\ ---- how long one operation is -----------------------------------------------
\ MEASURED AND NOT WRITTEN DOWN: the bytes the encoders really append are the
\ only statement of a size that cannot drift from them. The scratch sink holds
\ one operation and the flag keeps every displacement out of the answer, so a
\ measurement is the same number in both passes.
: OP-SIZE ( IR-ID:ir-op-id n -- n )
   {: id:IR-ID:ir-op-id home:n :}
   MEAS @ {: was:n :}
   1 MEAS !
   SC-CLEAR
   id home SC-SINK PUT-OP
   SC-LEN {: got:n :}
   was MEAS !
   got ;

\ Asked only about the operations BEFORE a block's terminator, so no position is
\ needed: an elided fall-through is the terminator's business.
: OP-SILENT? ( IR-ID:ir-op-id -- bool )
   -1 OP-SIZE 0= ;

: BLOCK-BYTES ( IR-ID:ir-block-id n -- n )
   {: bk:IR-ID:ir-block-id home:n :}
   0
   bk OP-COUNT 0 ?do
      bk i OP-AT home OP-SIZE +
   loop ;

\ ---- the blocks control only passes through ----------------------------------
\ Two operations can be present and write nothing, which is a fact about the
\ register assignment - so the ORDER is chosen after the acceptance is probed.
: SILENT-BEFORE-TERM? ( IR-ID:ir-block-id -- bool )
   {: bk:IR-ID:ir-block-id :}
   0
   bk OP-COUNT 1- 0 ?do
      bk i OP-AT OP-SILENT? 0= if 1+ then
   loop
   0= ;

: PASS-THRU? ( IR-ID:ir-fun-id n -- bool )
   {: f:IR-ID:ir-fun-id b:n :}
   b 0= if false exit then
   f b BLOCK-AT {: bk:IR-ID:ir-block-id :}
   bk TERM-AT {: t:IR-ID:ir-op-id :}
   t SLOT-AT O-BR <> if false exit then
   bk SILENT-BEFORE-TERM? ;

: CHASE-STEP ( IR-ID:ir-fun-id -- bool )
   {: f:IR-ID:ir-fun-id :}
   f CH-AT @ PASS-THRU? 0= if false exit then
   f CH-AT @ BLOCK-AT TERM-AT 0 SUCC-BLOCK {: nxt:n :}
   nxt CH-AT @ = if false exit then
   nxt CH-AT !
   true ;

\ A chain of pass-through blocks that closed into a loop would be walked for
\ ever, so the walk is bounded by the number of blocks there are.
: CHASE ( IR-ID:ir-fun-id n -- n )
   {: f:IR-ID:ir-fun-id b:n :}
   b CH-AT !
   N-BLK @ 0 ?do
      f CHASE-STEP 0= if leave then
   loop
   CH-AT @ ;

: GOTO! ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   N-BLK @ 0 ?do  f i CHASE  i cells B-GOTO + !  loop ;

\ ---- which blocks are still reached ------------------------------------------
: KEEP1 ( n -- n )
   {: s:n :}
   s KEPT? if 0 exit then
   1 s cells B-KEEP + !
   1 ;

: KEEP-SUCCS ( IR-ID:ir-fun-id n -- n )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT TERM-AT {: t:IR-ID:ir-op-id :}
   0
   t SUCCS-OF 0 ?do
      t i SUCC-BLOCK GOTO-OF KEEP1 +
   loop ;

: KEEP-SWEEP ( IR-ID:ir-fun-id -- n )
   {: f:IR-ID:ir-fun-id :}
   0
   N-BLK @ 0 ?do
      i KEPT? if f i KEEP-SUCCS + then
   loop ;

: KEEP! ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   N-BLK @ 0 ?do  0 i cells B-KEEP + !  loop
   1 0 cells B-KEEP + !
   begin  f KEEP-SWEEP 0=  until ;

\ Taken rather than assumed, because the trace fills exactly this many positions.
: KEPT-COUNT ( -- n )
   0
   N-BLK @ 0 ?do  i KEPT? if 1+ then  loop ;

: TAIL-BLOCK ( IR-ID:ir-block-id -- n )
   TERM-AT {: t:IR-ID:ir-op-id :}
   t SLOT-AT TAIL-SUCC {: s:n :}
   s 0 < if -1 exit then
   t s SUCC-BLOCK GOTO-OF ;

: NEXT-UNLAID ( -- n )
   0 begin dup N-BLK @ < while
      dup KEPT? over LAID? 0= and if exit then
      1+
   repeat
   drop -1 ;

: FOLLOWER ( IR-ID:ir-fun-id n -- n )
   {: f:IR-ID:ir-fun-id b:n :}
   f b BLOCK-AT TAIL-BLOCK {: s:n :}
   s 0 < if NEXT-UNLAID exit then
   s LAID? if NEXT-UNLAID exit then
   s ;

\ A routine every path of which traps has no return for the emission to end on,
\ so no block is pinned last and the trace decides the whole order.
-1 constant NO-RET

: RET-ORD ( IR-ID:ir-fun-id -- n )
   {: f:IR-ID:ir-fun-id :}
   NO-RET
   f BLOCK-COUNT 0 ?do
      f i BLOCK-AT TERM-AT {: t:IR-ID:ir-op-id :}
      t SUCCS-OF 0=  t SLOT-AT O-TRAP <>  and if
         dup NO-RET <> if E-X64EMIT-SHAPE throw then
         drop i
      then
   loop ;

\ A function's blocks are contiguous, which the successor arithmetic rests on,
\ so it is measured while it is filed rather than assumed.
: B-BASE! ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f 0 BLOCK-AT IR-ID:BLOCK-LOCAL B-BASE !
   f BLOCK-COUNT 0 ?do
      f i BLOCK-AT IR-ID:BLOCK-LOCAL  B-BASE @ -  i <>
      if E-X64EMIT-SHAPE throw then
   loop ;

: ORDER-NO-RET ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id k:n :}
   0 0 LAY
   k 1 ?do
      f  i 1- AT-POS  FOLLOWER  i LAY
   loop ;

\ The entry block first, the return block last, and the trace between them; a
\ block nothing reaches any more is left out of the order entirely and every
\ branch to a block that only passes control on was redirected past it.
: ORDER-BLOCKS ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f BLOCK-COUNT {: n:n :}
   n 1 < if E-X64EMIT-SHAPE throw then
   n N-BLK !
   f B-BASE!
   f GOTO!
   f KEEP!
   KEPT-COUNT {: k:n :}
   k N-LAID !
   f RET-ORD {: r:n :}
   n 0 ?do  -1 i cells B-PLACE + !  loop
   r NO-RET = if f k ORDER-NO-RET exit then
   r KEPT? 0= if E-X64EMIT-SHAPE throw then
   r k 1- LAY
   k 1 = if exit then
   r 0= if E-X64EMIT-SHAPE throw then
   0 0 LAY
   k 1- 1 ?do
      f  i 1- AT-POS  FOLLOWER  i LAY
   loop ;

\ ---- the shape this emitter writes from --------------------------------------
: FUN-AT ( n -- IR-ID:ir-fun-id )
   {: k:n :}
   k 0 < k N-FUNS @ >= or if E-X64EMIT-SHAPE throw then
   MKEY k IR-ID:PACK-FUN ;

: FUNS-CK ( -- )
   FUN-COUNT {: n:n :}
   n 1 < if E-X64EMIT-SHAPE throw then
   n N-FUNS ! ;

: TERMINATOR? ( IR-ID:ir-block-id n -- bool )
   OP-AT SLOT-AT {: k:n :}
   k O-RET = k O-BR = or k O-BRZ = or k O-CMPBR = or k O-CMPBRI = or
   k O-TAILCALL = or k O-TRAP = or ;

: BLOCK-CK ( IR-ID:ir-block-id -- )
   {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT {: n:n :}
   n 1 < if E-X64EMIT-SHAPE throw then
   bk n 1- TERMINATOR? 0= if E-X64EMIT-SHAPE throw then
   n 1- 0 ?do
      bk i TERMINATOR? if E-X64EMIT-SHAPE throw then
   loop ;

: SHAPE-CK ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   f BLOCK-COUNT 0 ?do
      f i BLOCK-AT BLOCK-CK
   loop ;

: SHAPES-CK ( -- )
   N-FUNS @ 0 ?do i FUN-AT SHAPE-CK loop ;

\ ---- what this run is told ---------------------------------------------------
: BOUND-CK ( -- )
   BND-MODE @ BOUND-YES <> if E-X64EMIT-MODULE throw then ;

: PLACED-CK ( -- )
   PLACE-MODE @ PLACE-YES <> if E-X64EMIT-STATE throw then ;

: BND-MODULE-CK ( IR-BUILD:module -- )
   IR-BUILD:FMODULE  0 BND-MOD @  IR-ID:MODULE-SAME?
   0= if E-X64EMIT-MODULE throw then ;

: DIALECT-CK ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b IR-BUILD:DIALECT@  X64IR:NAME IR-BUILD:SYMBOL-IS?
   0= if E-X64EMIT-MODULE throw then
   c b IR-BUILD:SCHEMA-MAJOR@ X64IR:MAJOR <> if E-X64EMIT-MODULE throw then
   c b IR-BUILD:SCHEMA-MINOR@ X64IR:MINOR <> if E-X64EMIT-MODULE throw then ;

\ THE ASSIGNMENT THIS EMISSION READS HAS TO BE THIS MODULE'S AND STILL FRESH.
\ Whether the allocator has an assignment at all is asked BEFORE the module it is
\ about, because a refused walk leaves the allocator with nothing to name a
\ module - and that refusal is this pass's, not the allocator's. The last probe
\ asks the validator whether the first value is in a register, which is the
\ question that refuses an acceptance a later allocation replaced.
: ACCEPT-CK ( IR-BUILD:module -- )
   {: m:IR-BUILD:module :}
   A64RAV:ACCEPTED? 0= if E-X64EMIT-ACCEPT throw then
   A64RA:SEALED? 0= if E-X64EMIT-ACCEPT throw then
   m IR-BUILD:FMODULE A64RA:MODULE@ IR-ID:MODULE-SAME?
   0= if E-X64EMIT-ACCEPT throw then
   A64RA:VALUES 0 > if 0 A64RAV:REGISTERED? drop then ;

\ Emission asks the registry for itself: a context whose machine no loaded
\ backend lowers for has no business producing this machine's bytes. It does NOT
\ prove the context is about x86-64 - the registry answers for whatever machine
\ the context names - and the module's own dialect, checked where it was bound,
\ is what says these bytes are this dialect's.
: TARGET-CK ( IR-CTX:ctx -- )
   X64IR:CHECK-TARGET ;

\ ---- the two passes ----------------------------------------------------------
\ MEASURE files where every block and every function starts, walking the same
\ order the writer will; WRITE-ALL writes and holds itself against those numbers.
\
\ EVERY FUNCTION IS LAID OUT TWICE, once in each pass, and the twin
\ src/compiler/native/emit.f lays its own twice for the same reason: the order
\ and the block starts are PER FUNCTION - B-ORDER, B-PLACE and B-START are all
\ indexed by a block's ordinal in the function it belongs to - so the numbers
\ MEASURE leaves in them are the LAST function's. The writer re-orders and
\ re-lays each function from the F-START that pass filed for it, and B-START is
\ about the function it is walking. A block start is still counted from the
\ start of the EMISSION and not of the function, because a displacement
\ subtracts two of them and both ends must share an origin.
: LAYOUT ( IR-ID:ir-fun-id n -- )
   {: f:IR-ID:ir-fun-id base:n :}
   base MCUR !
   N-LAID @ 0 ?do
      MCUR @  i AT-POS cells B-START + !
      MCUR @  f i AT-POS BLOCK-AT  i AT-POS BLOCK-BYTES  +  MCUR !
   loop ;

: MEASURE ( -- )
   0 MCUR !
   N-FUNS @ 0 ?do
      MCUR @  i cells F-START + !
      i FUN-AT {: f:IR-ID:ir-fun-id :}
      f ORDER-BLOCKS
      f  i cells F-START + @  LAYOUT
   loop ;

\ Where a block's bytes begin is what every displacement was measured against,
\ so the writer arriving elsewhere means two different routines.
: CURSOR-CK ( n -- )
   START-AT CUR <> if E-X64EMIT-LAYOUT throw then ;

: WALK-BLOCK ( IR-ID:ir-block-id n -- )
   {: bk:IR-ID:ir-block-id home:n :}
   bk OP-COUNT 0 ?do
      bk i OP-AT home EM-SINK PUT-OP
      bk i OP-AT SLOT-AT EM-LAST !
   loop ;

: WALK ( IR-ID:ir-fun-id -- )
   {: f:IR-ID:ir-fun-id :}
   N-LAID @ 0 ?do
      i AT-POS CURSOR-CK
      f i AT-POS BLOCK-AT  i AT-POS  WALK-BLOCK
   loop ;

: WRITE-ALL ( -- )
   N-FUNS @ 0 ?do
      i FUN-AT {: f:IR-ID:ir-fun-id :}
      f ORDER-BLOCKS
      f  i cells F-START + @  LAYOUT
      i cells F-START + @ CUR <> if E-X64EMIT-LAYOUT throw then
      f WALK
   loop
   CUR MCUR @ <> if E-X64EMIT-LAYOUT throw then ;

: SEAL-CK ( -- )
   ST @ ST-SEALED <> if E-X64EMIT-STATE throw then ;

: SITE-ORD-CK ( n -- n )
   dup 0 < over N-SITES @ >= or if E-X64EMIT-BOUND throw then ;

public

\ ---- binding the dialect -----------------------------------------------------
\ The only moment a module can be asked its opcode and key identities, because
\ its symbols are its own ordinals.
: BIND-DIALECT ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b DIALECT-CK
   X64IR:OPCODES 0 ?do
      c b i X64IR:BIND  i BND-OP !
   loop
   b IR-BUILD:MODULE@ 0 BND-MOD !
   c b X64IR:KEY-IMM    0 BND-IMM !
   c b X64IR:KEY-ADDR   0 BND-ADDR !
   c b X64IR:KEY-SHIFT  0 BND-SHIFT !
   c b X64IR:KEY-COND   0 BND-COND !
   c b X64IR:KEY-DSLOT  0 BND-DSLOT !
   c b X64IR:KEY-DBYTES 0 BND-DBYTES !
   c b X64IR:KEY-DBACK  0 BND-DBACK !
   c b X64IR:KEY-ENTRY  0 BND-ENTRY !
   BOUND-YES BND-MODE ! ;

: BOUND? ( -- bool )
   BND-MODE @ BOUND-YES = ;

: RELEASE ( -- )
   BOUND-NO BND-MODE !
   PLACE-NO PLACE-MODE !
   0 PLACE-AT-N ! ;

\ ---- declaring where this routine will be written ----------------------------
\ There is no default: an emission whose calls are measured from a placement it
\ was never given is refused by name. WHAT IS CHECKED IS THAT IT COULD BE AN
\ ENTRY OF THIS MACHINE. An x86-64 instruction needs no alignment to decode, so
\ this is not about decoding: the only alignment this machine states is
\ X64IR:SP-ALIGN, the sixteen bytes the SysV contract aligns to, and a code
\ region that hands out slots in that unit is one whose arithmetic stays whole.
\ A writer that packs routines tighter pads to it rather than placing between.
: PLACE-AT ( n -- )
   {: at:n :}
   PLACE-MODE @ PLACE-YES = if E-X64EMIT-PLACE throw then
   at 0 < if E-X64EMIT-PLACE throw then
   at X64IR:SP-ALIGN mod 0<> if E-X64EMIT-PLACE throw then
   at PLACE-AT-N !
   PLACE-YES PLACE-MODE ! ;

\ ---- the pass ----------------------------------------------------------------
\ The shape is a question about the module alone and is asked first; the
\ acceptance is about the assignment and is asked next; the order and the layout
\ come after both, because a block's size is a fact about the assignment.
: EMIT ( IR-CTX:ctx IR-BUILD:module -- )
   {: c:IR-CTX:ctx m:IR-BUILD:module :}
   BOUND-CK
   PLACED-CK
   SINK-READY
   EM-SINK BUF:CLEAR
   ST-EMPTY ST !
   0 N-SITES !
   0 MEAS !
   -1 EM-LAST !
   m BND-MODULE-CK
   c TARGET-CK
   m VIEWS!
   RESERVE-SCRATCH
   FUNS-CK
   SHAPES-CK
   m ACCEPT-CK
   MEASURE
   WRITE-ALL
   ST-SEALED ST ! ;

\ Retiring one definition is deliberately nonthrowing. The compiler calls it on
\ both the accepting and the rejecting path, so it gives back the placement a
\ refused run never spent as well as the bytes a sealed one holds.
\
\ THE MEASURING FLAG IS PART OF WHAT IS GIVEN BACK, and EMIT clears it again
\ before it walks: a refusal like E-X64EMIT-FORM is thrown from inside OP-SIZE,
\ which is to say from inside a measurement, and a flag left standing there
\ would make the NEXT emission's writer take the measuring arm of every
\ displacement and write a zero where a branch or a call belongs.
: RETIRE ( -- )
   ST-EMPTY ST !
   PLACE-NO PLACE-MODE !
   0 PLACE-AT-N !
   0 N-SITES !
   0 MEAS !
   -1 EM-LAST !
   SINK-MODE @ 0<> if EM-SINK BUF:CLEAR then ;

\ The registry releases buffers immediately before a DATA copy, so the mapped
\ sinks are given back here and taken again on the next emission.
: CAPTURE-PREPARE ( -- )
   RETIRE
   SINK-MODE @ 0= if exit then
   EM-SINK BUF:DISPOSE
   SC-SINK BUF:DISPOSE
   0 SINK-MODE ! ;

: RESET-SCRATCH ( -- )
   0 SCRATCH-BLOCKS ! 0 SCRATCH-FUNS ! 0 SCRATCH-OPS ! ;

\ ---- the sealed emission, all of it in BYTES ---------------------------------
\ Every reader below answers BYTES where the ARM64 emitter answers instruction
\ indices, because on this machine an instruction has no fixed width.
: SEALED? ( -- bool )
   ST @ ST-SEALED = ;

: SIZE ( -- n )
   SEAL-CK EM-LEN ;

: BYTES ( -- ptr u8 )
   SEAL-CK EM-SINK BUF:SPAN$ drop ;

\ The count of POSITIONS and not of blocks: the order leaves out the blocks every
\ branch was redirected past. BLOCK-START@ is still keyed by ORDINAL.
: BLOCKS ( -- n )
   SEAL-CK N-LAID @ ;

: BLOCK-START@ ( n -- n )
   SEAL-CK START-AT ;

: FUNCTION-OFFSET@ ( n -- n )
   SEAL-CK FUN-START ;

\ The last OPERATION is asked about rather than the last instruction: a routine
\ that leaves through a tail branch ends in no return at all.
: TRAILING-RETURN? ( -- bool )
   SEAL-CK EM-LAST @ O-RET = ;

: ADDR-SITES ( -- n )
   SEAL-CK N-SITES @ ;

\ The byte offset of the `mov r64, imm64` itself; its immediate begins
\ X64ASM:MOV-RI64-IMM-OFF bytes later, which is where the relocation writer
\ patches.
: ADDR-SITE@ ( n -- n )
   SEAL-CK SITE-ORD-CK cells SITES + @ ;

: ADDR-SITE-KIND@ ( n -- n )
   SEAL-CK SITE-ORD-CK cells SKIND + @ ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;using
;using
;package

\ emit-x64.f - the x86-64 bytes of one accepted straight-line leaf routine,
\ appended to the byte sink the caller owns.
\
\ IT IS THE TWIN OF src/compiler/native/emit.f AND NOT A PARAMETERISATION OF IT.
\ docs/x86-64.md "The emitter's sink" decided that before any emitter code: that
\ file is a FIXED-WIDTH word sink - four bytes an instruction, a capacity counted
\ in instructions, a source map keyed by instruction ordinal - and an x86-64
\ instruction is one to fifteen bytes. Here the sink is the lib/byte-buffer.f BUF
\ that every src/arch/x86-64/asm.f encoder already takes as its LAST operand, so
\ this pass appends and owns no code buffer at all.
\
\ WHAT THIS SLICE WRITES. One function of one block that ends in `x64.ret`, calls
\ nothing and reserves no frame: the literal, the copy, the five tied binaries
\ and their immediate forms, the two immediate shifts, the complement, the two
\ fused comparisons, the four data-stack crossings and the four addressed forms.
\ Everything else is refused BY NAME rather than guessed at - a branch, a divide,
\ a select, a call, a trap and a code address each have no byte here. There is no
\ layout pass, no rel32, no relaxation and no size query, because a single block
\ has no displacement to measure and nothing to patch; the later slices of dot
\ habu-emit-x86-64-1f31f6e3 add the layout that needs them.
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
\ peephole this slice does not have, and what the pinned bytes in
\ test/compiler/x64-emit.f measure is a faithful image of the module.

require lib/prelude.f
require lib/errors.f
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

\ ---- the bound dialect -------------------------------------------------------
\ A module's symbols are its own ordinals, so the identities are taken once from
\ the module about to be written and never spelled again.
0 constant BOUND-NO
1 constant BOUND-YES

here CELL 1- and CELL swap - CELL 1- and allot
variable BND-MODE
BOUND-NO BND-MODE !

1 TYPED-BUFFER BND-MOD IR-ID:ir-module-id
X64IR:OPCODES TYPED-BUFFER BND-OP IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-IMM IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-ADDR IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-SHIFT IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-COND IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-DSLOT IR-ID:ir-symbol-id
1 TYPED-BUFFER BND-DBYTES IR-ID:ir-symbol-id

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
\ that end control, the ones that are a call site and the ones that touch a
\ frame. They are named here by ordinal because the shape check reads ordinals.
X64IR-OPCODE:RET      X64IR:ORD constant O-RET
X64IR-OPCODE:BR       X64IR:ORD constant O-BR
X64IR-OPCODE:BRZ      X64IR:ORD constant O-BRZ
X64IR-OPCODE:CMPBR    X64IR:ORD constant O-CMPBR
X64IR-OPCODE:CMPBRI   X64IR:ORD constant O-CMPBRI
X64IR-OPCODE:TAILCALL X64IR:ORD constant O-TAILCALL
X64IR-OPCODE:TRAP     X64IR:ORD constant O-TRAP
X64IR-OPCODE:CALL     X64IR:ORD constant O-CALL
X64IR-OPCODE:WORDCALL X64IR:ORD constant O-WORDCALL
X64IR-OPCODE:RESERVE  X64IR:ORD constant O-RESERVE
X64IR-OPCODE:RELEASE  X64IR:ORD constant O-RELEASE
X64IR-OPCODE:STORE    X64IR:ORD constant O-STORE
X64IR-OPCODE:LOAD     X64IR:ORD constant O-LOAD

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

\ ---- one operation, written --------------------------------------------------
: PUT-MOVI ( IR-ID:ir-op-id ptr a -- )
   {: id:IR-ID:ir-op-id s:ptr :}
   id ADDR-OF X64IR:ADDR-NONE <> if E-X64EMIT-FORM throw then
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

\ ---- the dispatch ------------------------------------------------------------
\ Every opcode of the dialect is named, so a form added to the vocabulary is a
\ decision taken HERE rather than a silent fall-through. The refusing arms are
\ the forms outside this slice: they need a layout, a relocation site, a fixed
\ register or a frame, and none of those exists yet.
: PUT-OP ( IR-ID:ir-op-id ptr a -- )
   {: id:IR-ID:ir-op-id s:ptr :}
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
      br       OF E-X64EMIT-FORM throw ENDOF
      brz      OF E-X64EMIT-FORM throw ENDOF
      cmpbr    OF E-X64EMIT-FORM throw ENDOF
      cmpbri   OF E-X64EMIT-FORM throw ENDOF
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
      call     OF E-X64EMIT-FORM throw ENDOF
      wordcall OF E-X64EMIT-FORM throw ENDOF
      tailcall OF E-X64EMIT-FORM throw ENDOF
      trap     OF E-X64EMIT-FORM throw ENDOF
      codeaddr OF E-X64EMIT-FORM throw ENDOF
   ;MATCH ;

\ ---- the shape this emitter writes from --------------------------------------
\ One function, one block, one return at its end, no call site and no frame.
\ Everything the check refuses needs a pass this slice has not got: a second
\ block needs the layout a displacement is measured against, a call needs a
\ placement to measure from, and a frame needs a prologue.
: FUN0 ( -- IR-ID:ir-fun-id )
   MKEY 0 IR-ID:PACK-FUN ;

: TERM? ( n -- bool )
   {: k:n :}
   k O-RET = k O-BR = or k O-BRZ = or k O-CMPBR = or k O-CMPBRI = or
   k O-TAILCALL = or k O-TRAP = or ;

: CALL-FORM? ( n -- bool )
   {: k:n :}
   k O-CALL = k O-WORDCALL = or k O-TAILCALL = or ;

: FRAME-FORM? ( n -- bool )
   {: k:n :}
   k O-RESERVE = k O-RELEASE = or k O-STORE = or k O-LOAD = or ;

: BODY-OP-CK ( IR-ID:ir-block-id n -- )
   OP-AT SLOT-AT {: k:n :}
   k TERM? if E-X64EMIT-SHAPE throw then
   k CALL-FORM? if E-X64EMIT-SHAPE throw then
   k FRAME-FORM? if E-X64EMIT-SHAPE throw then ;

: BLOCK0 ( -- IR-ID:ir-block-id )
   FUN-COUNT 1 <> if E-X64EMIT-SHAPE throw then
   FUN0 BLOCK-COUNT 1 <> if E-X64EMIT-SHAPE throw then
   FUN0 0 BLOCK-AT ;

: SHAPE-CK ( -- )
   BLOCK0 {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT {: n:n :}
   n 1 < if E-X64EMIT-SHAPE throw then
   bk n 1- OP-AT SLOT-AT O-RET <> if E-X64EMIT-SHAPE throw then
   n 1- 0 ?do
      bk i BODY-OP-CK
   loop ;

\ ---- what this run is told ---------------------------------------------------
: BOUND-CK ( -- )
   BND-MODE @ BOUND-YES <> if E-X64EMIT-MODULE throw then ;

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
   BOUND-YES BND-MODE ! ;

\ ---- the pass ----------------------------------------------------------------
\ The shape is a question about the module alone and is asked first; the
\ acceptance is about the assignment and is asked next; only then is a byte
\ appended, so a refused emission leaves the sink as it found it.
: EMIT ( IR-BUILD:module ptr a -- )
   {: m:IR-BUILD:module s:ptr :}
   BOUND-CK
   m BND-MODULE-CK
   m VIEWS!
   SHAPE-CK
   m ACCEPT-CK
   BLOCK0 {: bk:IR-ID:ir-block-id :}
   bk OP-COUNT 0 ?do
      bk i OP-AT s PUT-OP
   loop ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;using
;using
;package

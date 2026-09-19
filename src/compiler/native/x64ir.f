\ x64ir.f - the x86-64 machine dialect: the closed set of operations that stand
\ for real x86-64 instruction forms, with virtual registers as SSA values. It is
\ to src/arch/x86-64/asm.f what src/compiler/native/a64ir.f is to the ARM64
\ assembler, and the three ways it is NOT a transcription of that file are the
\ three facts about this machine a lowering has to obey.
\
\ ONE. MOST FORMS ARE TWO-ADDRESS. `add rd, rs` overwrites rd; there is no
\ three-register add. Every such operation therefore declares a TIE between its
\ result and the operand it destroys (src/compiler/ir/schema.f ADD-TIE), which is
\ the constraint a register allocator reads rather than a rule it must know per
\ opcode. The allocator REFUSES a tie whose ends cannot share a register rather
\ than repairing it, so the selector is what makes a tie satisfiable: it copies
\ the operand a form is about to destroy whenever another use reads it
\ (select-x64.f, "selecting the arithmetic"). Where ARM64 has `madd`, this
\ machine has nothing: `imul rd, rs` is the tied form and an addend is a second
\ operation.
\
\ TWO. THE FLAGS ARE NOT A VALUE. `cmp` writes RFLAGS and `setcc`, `cmovcc` and
\ `jcc` read them, and no SSA value can stand for a single architectural
\ resource the allocator would otherwise have to hand out. So every use of the
\ flags is FUSED into one operation the way a64.cmpbr fuses compare and branch:
\ x64.cmpset is compare-and-set-a-boolean, x64.cmpsel is compare-and-move,
\ x64.cmpbr is compare-and-branch, x64.brz is test-and-branch. Nothing in this
\ dialect leaves flags live across an operation boundary.
\
\ THREE. THE LITERAL IS ONE INSTRUCTION AND THE CALL LEAVES NO LINK REGISTER.
\ ARM64 builds a constant from a movz/movk run and keeps its return address in
\ x30, which is why a64ir has `movk`, `linksave` and `linkload`. Here a literal
\ is `mov r64, imm64` - ten bytes, one relocation site, patched at offset 2, the
\ site kind src/habu/aot-decl.f calls MOVABS - and `call` pushes the return
\ address on the machine stack, so there is no link register to spill and no
\ opcode for spilling it. For the same reason there is no fused push or pop: x86
\ has no write-back addressing mode, so a data-stack store and the pointer move
\ after it are two operations and a64.dpush/a64.dpop have no counterpart here.
\
\ WHAT IS NOT HERE YET. The floating forms. The machine's own floating registers
\ are described (this dialect's REGFILE declares sixteen), because the
\ description is the machine's and not the opcode set's, but no SSE operation is
\ declared, so a compilation that reaches an HIR float operation is refused by
\ the selector rather than lowered wrongly. They are the next addition to this
\ vocabulary and they raise MINOR when they land.

require lib/prelude.f
require lib/errors.f
require src/compiler/target.f
require src/compiler/binding.f
require src/compiler/ir/id.f
require src/compiler/ir/context.f
require src/compiler/ir/type.f
require src/compiler/ir/schema.f
require src/compiler/ir/symbol.f
require src/compiler/ir/build.f
require src/arch/x86-64/asm.f
require src/arch/x86-64/backend.f
require src/compiler/native/regfile.f

package X64IR
public

\ An ENUM, so a selection rule cannot name an operation this dialect does not
\ have and every MATCH over it has to answer for every member.
ENUM opcode DERIVE eq
   movi
   mov
   add
   sub
   and
   or
   xor
   imul
   addi
   subi
   andi
   ori
   xori
   shli
   shri
   shl
   shr
   neg
   not
   idiv
   cmpset
   cmpseti
   cmpsel
   selz
   br
   brz
   cmpbr
   cmpbri
   ret
   reserve
   release
   store
   load
   dtake
   dload
   dstore
   dpublish
   aload
   astore
   abload
   abstore
   call
   wordcall
   tailcall
   trap
   codeaddr
;ENUM

\ One condition per SOURCE relation, so a lowering is never an operand order in
\ one place and a condition in another. `equal` is spelled so because the ENUM's
\ derived comparison word takes `eq`. There are six and not seven: the seventh
\ ARM64 condition exists for the float compares, which this dialect has not got.
ENUM cond DERIVE eq
   lt
   le
   gt
   ge
   equal
   ne
;ENUM

private

\ ---- the machine bounds ------------------------------------------------------
64 constant XBITS                    \ bits in a general register
16 constant GPRS-N                   \ rax..r15
16 constant FPRS-N                   \ xmm0..xmm15

\ The signed immediate every ALU form, every compare and every displacement
\ carries. x86-64 sign-extends imm32 into the 64-bit operand; there is no wider
\ immediate outside `mov r64, imm64`, which carries a whole cell and therefore
\ has no bound at all.
32 constant IMM-BITS
1 IMM-BITS 1- lshift constant IMM-LIM    \ 2^31: the magnitude either sign holds

\ The count field a shift carries, and the count the machine itself honours:
\ `shl r64, imm8` masks the count to six bits, so a count outside them is a
\ selector fault and not a form this dialect can carry.
6 constant SHIFT-BITS
1 SHIFT-BITS lshift constant SHIFT-LIM

\ ---- the frame bounds --------------------------------------------------------
XBITS 8 / constant SLOT-BYTES        \ bytes one frame access moves
16 constant SP-ALIGN-N               \ the SysV stack alignment this port keeps

\ The deepest frame this dialect describes is the deepest one its displacement
\ reaches, rounded down to the alignment: the bound is the FIELD's and not a
\ policy's, so a policy that wants a smaller frame says so where it is declared.
IMM-LIM 1- constant DISP-MAX
DISP-MAX dup SP-ALIGN-N mod - constant FRAME-LIM

\ ---- the condition field -----------------------------------------------------
\ The four-bit tttn the Jcc, SETcc and CMOVcc opcodes carry. Each code is TAKEN
\ from the shipped assembler's own word rather than written again here, so the
\ two cannot drift: the signed conditions are C-L, C-LE, C-G and C-GE - never
\ the unsigned C-B/C-A family, which is what a comparison of Habu cells would be
\ wrong under - and equality is C-E and C-NE.
4 constant COND-BITS
1 COND-BITS lshift constant COND-LIM
X64ASM:C-L X64ASM:CONDITION>N constant COND-LT
X64ASM:C-LE X64ASM:CONDITION>N constant COND-LE
X64ASM:C-G X64ASM:CONDITION>N constant COND-GT
X64ASM:C-GE X64ASM:CONDITION>N constant COND-GE
X64ASM:C-E X64ASM:CONDITION>N constant COND-EQ
X64ASM:C-NE X64ASM:CONDITION>N constant COND-NE

\ ---- the registers the virtual machine holds ---------------------------------
\ docs/x86-64.md fixes five of them and the interpreter register; rsp is the
\ sixth of the machine's own and the seventh of this set, because `call` pushes
\ the return address through it and a routine that allocated it would destroy
\ its own return. ARM64 has no equivalent slip to make: its stack pointer is not
\ one of the thirty-one general registers an operand field can name.
3 constant R-RBX                     \ the interpreter register
4 constant R-RSP                     \ the machine stack pointer
5 constant R-RBP                     \ the user area
12 constant R-DSP                    \ r12, the data-stack pointer
13 constant R-DBASE                  \ r13, the data base
14 constant R-DICT                   \ r14, the dictionary
15 constant R-CODE                   \ r15, the code pointer

: RESERVED-MASK ( -- n )
   1 R-RBX lshift
   1 R-RSP lshift or
   1 R-RBP lshift or
   1 R-DSP lshift or
   1 R-DBASE lshift or
   1 R-DICT lshift or
   1 R-CODE lshift or ;

: GPR-MASK ( -- n )   1 GPRS-N lshift 1- ;

: FPR-MASK ( -- n )   1 FPRS-N lshift 1- ;

\ ---- the dialect's own symbols -----------------------------------------------

: TARGET ( -- )
   CTARGET-ARCH:X86-64 CTARGET:F-BASE IR-SCHEMA:SET-TARGET ;

\ A value-producing machine operation ends no block, names no successor, holds
\ no region and carries no effect token.
: PURE-VALUE ( -- )
   false 0 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE ;

: TOTAL ( -- )
   false IR-SCHEMA:SET-TRAP ;

public

\ ---- the dialect identity ----------------------------------------------------
: NAME ( -- ptr u8 n )
   s" x64" ;

\ Every consumer compares the version exactly, so a table with a form and one
\ without are two different tables. The floating forms raise MINOR.
0 constant MAJOR
1 constant MINOR

\ ---- the machine bounds, for a consumer that has to agree with them -----------
: REG-BITS ( -- n )        XBITS ;
: IMM-LIMIT ( -- n )       IMM-LIM ;
: SHIFT-LIMIT ( -- n )     SHIFT-LIM ;
: DISP-LIMIT ( -- n )      DISP-MAX ;
: FRAME-LIMIT ( -- n )     FRAME-LIM ;
: SP-ALIGN ( -- n )        SP-ALIGN-N ;

\ A small enumeration rather than a flag, so the CODE kind is a new member. The
\ kinds are ARM64's, because what they classify - which relocation site a
\ literal opens - is the relocation model's fact and not the machine's.
0 constant ADDR-NONE
1 constant ADDR-DATA
2 constant ADDR-CODE
ADDR-CODE constant ADDR-KIND-MAX

private

\ ---- checked immediate operands ----------------------------------------------
\ The one form that carries a whole cell, `mov r64, imm64`, is checked nowhere:
\ every 64-bit value fits it, which is the whole reason it is this machine's
\ relocatable literal.
: IMM32 ( n -- n )
   dup IMM-LIM negate < over IMM-LIM 1- > or if E-X64IR-IMM throw then ;

: SHIFT-COUNT ( n -- n )
   dup 0 < over SHIFT-LIM >= or if E-X64IR-SHIFT throw then ;

\ An unknown number would reach the emitter as a site class it has no rule for.
: ADDR-KIND ( n -- n )
   dup 0 < over ADDR-KIND-MAX > or if E-X64IR-IMM throw then ;

\ ---- checked frame operands --------------------------------------------------
\ A frame slot is addressed at a non-negative displacement from the frame
\ pointer this port uses for its own frame, in whole cells, inside disp32.
: SLOT ( n -- n )
   dup 0 < if E-X64IR-SLOT throw then
   dup SLOT-BYTES mod 0<> if E-X64IR-SLOT throw then
   dup DISP-MAX > if E-X64IR-SLOT throw then ;

\ The stack pointer stays aligned and the frame stays inside the displacement
\ that reaches it.
: FRAME ( n -- n )
   dup 0 < if E-X64IR-FRAME throw then
   dup SP-ALIGN-N mod 0<> if E-X64IR-FRAME throw then
   dup FRAME-LIM > if E-X64IR-FRAME throw then ;

\ ---- checked data-stack operands ---------------------------------------------
\ SIGNED, unlike the ARM64 field: a cell already pushed is at a negative
\ displacement from the pointer and x86-64 displacements are signed, so there is
\ no separate bound behind the pointer to state.
: DSLOT ( n -- n )
   dup SLOT-BYTES mod 0<> if E-X64IR-DSLOT throw then
   dup abs DISP-MAX > if E-X64IR-DSLOT throw then ;

\ A whole number of cells, either sign, inside the add and subtract immediate.
: DBYTES ( n -- n )
   dup SLOT-BYTES mod 0<> if E-X64IR-DBYTES throw then
   dup abs IMM-LIM 1- > if E-X64IR-DBYTES throw then ;

\ How far away it is is not asked here: the distance depends on where the
\ CALLING routine is written, so the reach stays the emitter's. There is no
\ alignment to check - an x86-64 instruction may begin at any byte.
: ENTRY ( n -- n )
   dup 0 <= if E-X64IR-ENTRY throw then ;

\ ---- the checked function ordinal --------------------------------------------
: FUN-ORD ( n -- n )
   dup 0 < if E-X64IR-FUN throw then ;

\ ---- the checked condition operand -------------------------------------------
: COND ( n -- n )
   dup 0 < over COND-LIM >= or if E-X64IR-COND throw then ;

public

\ ---- the machine this compilation is for --------------------------------------
\ The contract the context is bound to, which is what both of this backend's
\ stage gates ask the registry about.
: CONTRACT@ ( IR-CTX:ctx -- CTARGET:contract )
   IR-CTX:BINDING@ CBIND:VALIDATE CBIND:TARGET@ ;

\ A coherent foreign target can own HIR; producing a machine module for it is a
\ different question, and it is the registry's. An architecture whose backend is
\ not loaded in this image refuses there with E-CTGT-UNLOADED; a loaded backend
\ that does not serve this machine refuses here. Refuse before allocating.
: CHECK-TARGET ( IR-CTX:ctx -- )
   CONTRACT@ CTARGET:LOWERS? 0= if E-IR-SCHEMA-TARGET throw then ;

: GPR-TYPE ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   IR--TYPE-WIDTH:W64 IR--TYPE-SIGN:SIGNED IR-BUILD:INTERN-INT ;

\ ---- the type of the memory token --------------------------------------------
: MEM-TYPE ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   IR--TYPE-DOMAIN:DATA-MEM IR-BUILD:INTERN-TOKEN ;

\ The machine really has two register files: an instruction naming an XMM
\ register cannot name a general one in the same field.
: FPR-TYPE ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   IR--TYPE-FMT:DOUBLE IR-BUILD:INTERN-FLT ;

\ ---- the bytes one frame access moves ----------------------------------------
: SLOT-WIDTH ( -- n )    SLOT-BYTES ;

\ ---- the register a frame access is taken from -------------------------------
\ rsp, which is one of the sixteen general registers here and is therefore named
\ by number rather than implied by a form. src/arch/x86-64/machine.f states it
\ as the machine's stack pointer; the number is this file's, because the whole
\ reserved set above is.
: SP-GPR ( -- n )        R-RSP ;

\ ---- this machine's register files, for the passes that are not about it ------
\ The register allocator is linear scan and not an x86-64 pass, so it reads this
\ description instead of the numbers around it. Sixteen general registers, seven
\ of them the virtual machine's (rbx, rsp, rbp, r12..r15), so nine are
\ allocatable; sixteen floating with none reserved; eight-byte slots.
\
\ Every allocatable register is declared call-destroyed because that is what the
\ Habu convention says: src/compiler/native/abi.f declares the whole pool
\ destroyed on every routine, so no value survives a call in a register and the
\ callee-saved set this description derives is empty. Under SysV the machine's
\ callee-saved set is rbx, rbp and r12..r15 - exactly the reserved set - so a
\ foreign call preserves this virtual machine with no save block, and that fact
\ costs this description nothing to state: the registers it would save are the
\ ones it never hands out.
: REGFILE ( -- NREGFILE:file )
   GPRS-N
   RESERVED-MASK NREGFILE:REGS-SET
   GPR-MASK RESERVED-MASK xor NREGFILE:REGS-SET
   FPRS-N
   NREGFILE:REGS-NONE
   FPR-MASK NREGFILE:REGS-SET
   SLOT-BYTES
   NREGFILE:FILE ;

private

\ ---- the opcode names --------------------------------------------------------
\ One table and not a literal at each use, because a session interns this
\ dialect's whole vocabulary by walking it: a spelling the walk cannot reach is
\ a spelling every module interns again.
: OP-NAME ( X64IR:opcode -- ptr u8 n )
   MATCH opcode
      movi     OF s" x64.movi"     ENDOF
      mov      OF s" x64.mov"      ENDOF
      add      OF s" x64.add"      ENDOF
      sub      OF s" x64.sub"      ENDOF
      and      OF s" x64.and"      ENDOF
      or       OF s" x64.or"       ENDOF
      xor      OF s" x64.xor"      ENDOF
      imul     OF s" x64.imul"     ENDOF
      addi     OF s" x64.addi"     ENDOF
      subi     OF s" x64.subi"     ENDOF
      andi     OF s" x64.andi"     ENDOF
      ori      OF s" x64.ori"      ENDOF
      xori     OF s" x64.xori"     ENDOF
      shli     OF s" x64.shli"     ENDOF
      shri     OF s" x64.shri"     ENDOF
      shl      OF s" x64.shl"      ENDOF
      shr      OF s" x64.shr"      ENDOF
      neg      OF s" x64.neg"      ENDOF
      not      OF s" x64.not"      ENDOF
      idiv     OF s" x64.idiv"     ENDOF
      cmpset   OF s" x64.cmpset"   ENDOF
      cmpseti  OF s" x64.cmpseti"  ENDOF
      cmpsel   OF s" x64.cmpsel"   ENDOF
      selz     OF s" x64.selz"     ENDOF
      br       OF s" x64.br"       ENDOF
      brz      OF s" x64.brz"      ENDOF
      cmpbr    OF s" x64.cmpbr"    ENDOF
      cmpbri   OF s" x64.cmpbri"   ENDOF
      ret      OF s" x64.ret"      ENDOF
      reserve  OF s" x64.reserve"  ENDOF
      release  OF s" x64.release"  ENDOF
      store    OF s" x64.store"    ENDOF
      load     OF s" x64.load"     ENDOF
      dtake    OF s" x64.dtake"    ENDOF
      dload    OF s" x64.dload"    ENDOF
      dstore   OF s" x64.dstore"   ENDOF
      dpublish OF s" x64.dpublish" ENDOF
      aload    OF s" x64.aload"    ENDOF
      astore   OF s" x64.astore"   ENDOF
      abload   OF s" x64.abload"   ENDOF
      abstore  OF s" x64.abstore"  ENDOF
      call     OF s" x64.call"     ENDOF
      wordcall OF s" x64.wordcall" ENDOF
      tailcall OF s" x64.tailcall" ENDOF
      trap     OF s" x64.trap"     ENDOF
      codeaddr OF s" x64.codeaddr" ENDOF
   ;MATCH ;

public

\ ---- the closed opcode vocabulary -------------------------------------------
\ The ordinal is what the passes store in their own tables, so it is stated
\ once here and never derived from the enum's declaration order.
46 constant OPCODES

: ORD ( X64IR:opcode -- n )
   MATCH opcode
      movi     OF 0  ENDOF
      mov      OF 1  ENDOF
      add      OF 2  ENDOF
      sub      OF 3  ENDOF
      and      OF 4  ENDOF
      or       OF 5  ENDOF
      xor      OF 6  ENDOF
      imul     OF 7  ENDOF
      addi     OF 8  ENDOF
      subi     OF 9  ENDOF
      andi     OF 10 ENDOF
      ori      OF 11 ENDOF
      xori     OF 12 ENDOF
      shli     OF 13 ENDOF
      shri     OF 14 ENDOF
      shl      OF 15 ENDOF
      shr      OF 16 ENDOF
      neg      OF 17 ENDOF
      not      OF 18 ENDOF
      idiv     OF 19 ENDOF
      cmpset   OF 20 ENDOF
      cmpseti  OF 21 ENDOF
      cmpsel   OF 22 ENDOF
      selz     OF 23 ENDOF
      br       OF 24 ENDOF
      brz      OF 25 ENDOF
      cmpbr    OF 26 ENDOF
      cmpbri   OF 27 ENDOF
      ret      OF 28 ENDOF
      reserve  OF 29 ENDOF
      release  OF 30 ENDOF
      store    OF 31 ENDOF
      load     OF 32 ENDOF
      dtake    OF 33 ENDOF
      dload    OF 34 ENDOF
      dstore   OF 35 ENDOF
      dpublish OF 36 ENDOF
      aload    OF 37 ENDOF
      astore   OF 38 ENDOF
      abload   OF 39 ENDOF
      abstore  OF 40 ENDOF
      call     OF 41 ENDOF
      wordcall OF 42 ENDOF
      tailcall OF 43 ENDOF
      trap     OF 44 ENDOF
      codeaddr OF 45 ENDOF
   ;MATCH ;

: NTH ( n -- X64IR:opcode )
   case
      0  of X64IR-OPCODE:MOVI     endof
      1  of X64IR-OPCODE:MOV      endof
      2  of X64IR-OPCODE:ADD      endof
      3  of X64IR-OPCODE:SUB      endof
      4  of X64IR-OPCODE:AND      endof
      5  of X64IR-OPCODE:OR       endof
      6  of X64IR-OPCODE:XOR      endof
      7  of X64IR-OPCODE:IMUL     endof
      8  of X64IR-OPCODE:ADDI     endof
      9  of X64IR-OPCODE:SUBI     endof
      10 of X64IR-OPCODE:ANDI     endof
      11 of X64IR-OPCODE:ORI      endof
      12 of X64IR-OPCODE:XORI     endof
      13 of X64IR-OPCODE:SHLI     endof
      14 of X64IR-OPCODE:SHRI     endof
      15 of X64IR-OPCODE:SHL      endof
      16 of X64IR-OPCODE:SHR      endof
      17 of X64IR-OPCODE:NEG      endof
      18 of X64IR-OPCODE:NOT      endof
      19 of X64IR-OPCODE:IDIV     endof
      20 of X64IR-OPCODE:CMPSET   endof
      21 of X64IR-OPCODE:CMPSETI  endof
      22 of X64IR-OPCODE:CMPSEL   endof
      23 of X64IR-OPCODE:SELZ     endof
      24 of X64IR-OPCODE:BR       endof
      25 of X64IR-OPCODE:BRZ      endof
      26 of X64IR-OPCODE:CMPBR    endof
      27 of X64IR-OPCODE:CMPBRI   endof
      28 of X64IR-OPCODE:RET      endof
      29 of X64IR-OPCODE:RESERVE  endof
      30 of X64IR-OPCODE:RELEASE  endof
      31 of X64IR-OPCODE:STORE    endof
      32 of X64IR-OPCODE:LOAD     endof
      33 of X64IR-OPCODE:DTAKE    endof
      34 of X64IR-OPCODE:DLOAD    endof
      35 of X64IR-OPCODE:DSTORE   endof
      36 of X64IR-OPCODE:DPUBLISH endof
      37 of X64IR-OPCODE:ALOAD    endof
      38 of X64IR-OPCODE:ASTORE   endof
      39 of X64IR-OPCODE:ABLOAD   endof
      40 of X64IR-OPCODE:ABSTORE  endof
      41 of X64IR-OPCODE:CALL     endof
      42 of X64IR-OPCODE:WORDCALL endof
      43 of X64IR-OPCODE:TAILCALL endof
      44 of X64IR-OPCODE:TRAP     endof
      45 of X64IR-OPCODE:CODEADDR endof
      E-X64IR-OPCODE throw
   endcase ;

private

\ ---- the dialect's fixed attribute keys --------------------------------------
\ Enumerable for the same reason the opcode names are: a session prototype
\ interns this vocabulary by walking it.
0 constant K-IMM
1 constant K-ADDR
2 constant K-SHIFT
3 constant K-SLOT
4 constant K-FRAME
5 constant K-DSLOT
6 constant K-DBYTES
7 constant K-DBACK
8 constant K-ENTRY
9 constant K-TRAP-ENTRY
10 constant K-FUN
11 constant K-COND
12 constant KEYS

: KEY-NAME ( n -- ptr u8 n )
   case
      K-IMM        of s" x64.imm" endof
      K-ADDR       of s" x64.addr" endof
      K-SHIFT      of s" x64.shift" endof
      K-SLOT       of s" x64.slot" endof
      K-FRAME      of s" x64.frame" endof
      K-DSLOT      of s" x64.dslot" endof
      K-DBYTES     of s" x64.dbytes" endof
      K-DBACK      of s" x64.dback" endof
      K-ENTRY      of s" x64.entry" endof
      K-TRAP-ENTRY of s" x64.trap-entry" endof
      K-FUN        of s" x64.fun" endof
      K-COND       of s" x64.cond" endof
      E-X64IR-DIALECT throw
   endcase ;

\ ---- the vocabulary memo -----------------------------------------------------
\ A symbol identity is a (module, ordinal) pair and the module half changes with
\ every definition, so the ordinal is the only half worth remembering. The memo
\ holds one ordinal per vocabulary entry - every opcode name and every attribute
\ key - together with the module they belong to; a hit mints the identity with
\ IR-ID:PACK-SYMBOL against that module's own key and still puts it through
\ IR-BUILD:SYMBOL-CK, so a stale builder, a frozen one, a foreign context and a
\ row the table does not hold are refused exactly as they were.
\
\ A MODULE CLONED FROM THE SESSION PROTOTYPE ADOPTS THE WHOLE MEMO AT BIRTH,
\ because a clone holds the prototype's spellings at the prototype's ordinals.
OPCODES KEYS + constant VOCAB        \ memo entries: the opcodes, then the keys
VOCAB TYPED-BUFFER PROTO-ORD n       \ ordinals in the session prototype
VOCAB TYPED-BUFFER MEMO-ORD n        \ ordinals in the module the memo names
VOCAB TYPED-BUFFER MEMO-SEEN bool
1 TYPED-BUFFER MEMO-MOD IR-ID:ir-module-id
variable MEMO-OWNED                  \ MEMO-MOD names a module
variable MISS-COUNT

: MEMO-FORGET ( -- )
   0 MEMO-OWNED !
   VOCAB 0 ?do false i MEMO-SEEN ! loop ;
MEMO-FORGET
0 MISS-COUNT !

\ Whose ordinals the memo holds. Answering no is the per-hit owner check: the
\ ordinals of another module name other spellings, or nothing at all.
: MEMO-MINE? ( IR-ID:ir-module-id -- bool )
   MEMO-OWNED @ 0= if drop false exit then
   0 MEMO-MOD @ IR-ID:MODULE-SAME? ;

: MEMO-START ( IR-ID:ir-module-id -- )
   0 MEMO-MOD !
   1 MEMO-OWNED !
   VOCAB 0 ?do false i MEMO-SEEN ! loop ;

\ Adopt the prototype's ordinals for a module that was cloned from it.
: MEMO-ADOPT ( IR-BUILD:builder -- )
   IR-BUILD:MODULE@ 0 MEMO-MOD !
   1 MEMO-OWNED !
   VOCAB 0 ?do
      i PROTO-ORD @ i MEMO-ORD !
      true i MEMO-SEEN !
   loop ;

\ The spelling a memo entry names, in the order PROTOTYPE walked and recorded.
: VOCAB-NAME ( n -- ptr u8 n )
   {: i:n :}
   i OPCODES < if i NTH OP-NAME exit then
   i OPCODES - KEY-NAME ;

\ One entry of the memo, read or filled.
: MEMO-BIND ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder idx:n :}
   b IR-BUILD:MODULE@ {: owner:IR-ID:ir-module-id :}
   owner MEMO-MINE? 0= if owner MEMO-START then
   idx MEMO-SEEN @ if
      b IR-BUILD:MODULE-KEY idx MEMO-ORD @ IR-ID:PACK-SYMBOL
      {: prior:IR-ID:ir-symbol-id :}
      c b prior IR-BUILD:SYMBOL-CK
      prior exit
   then
   1 MISS-COUNT +!
   c b idx VOCAB-NAME IR-BUILD:INTERN-SYMBOL {: sym:IR-ID:ir-symbol-id :}
   sym IR-ID:SYMBOL-LOCAL idx MEMO-ORD !
   true idx MEMO-SEEN !
   sym ;

\ The keys sit above the opcodes in the one ordinal space.
: KEY-BIND ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:n :}
   c b OPCODES k + MEMO-BIND ;

public

\ Vocabulary bindings this dialect had to intern instead of mint. Every module
\ of a session is a clone, so a load past its first definition reports none.
: MISSES ( -- n ) MISS-COUNT @ ;

: MISSES-CLEAR ( -- ) 0 MISS-COUNT ! ;

: BIND ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder idx:n :}
   idx NTH drop                       \ an ordinal this dialect has an opcode for
   c b idx MEMO-BIND ;

\ Interning deduplicates, so asking twice answers the same identity - and the
\ memo above answers most of them without interning at all.
: OPCODE ( IR-CTX:ctx IR-BUILD:builder X64IR:opcode -- IR-ID:ir-symbol-id )
   ORD BIND ;

\ ---- the condition a comparison is made under --------------------------------
: COND-CODE ( X64IR:cond -- n )
   MATCH cond
      lt    OF COND-LT ENDOF
      le    OF COND-LE ENDOF
      gt    OF COND-GT ENDOF
      ge    OF COND-GE ENDOF
      equal OF COND-EQ ENDOF
      ne    OF COND-NE ENDOF
   ;MATCH ;

: N>COND ( n -- X64IR:cond )
   case
      COND-LT of X64IR-COND:LT endof
      COND-LE of X64IR-COND:LE endof
      COND-GT of X64IR-COND:GT endof
      COND-GE of X64IR-COND:GE endof
      COND-EQ of X64IR-COND:EQUAL endof
      COND-NE of X64IR-COND:NE endof
      E-X64IR-COND throw
   endcase ;

\ ---- the keys and the checked attributes they carry --------------------------
: KEY-IMM ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   K-IMM KEY-BIND ;

\ A literal starting an address chain has to be found again after publication,
\ and a relocation pass may not decode region bytes. It is a REQUIRED key, so a
\ rewrite that drops it stops the compilation.
: KEY-ADDR ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   K-ADDR KEY-BIND ;

: KEY-SHIFT ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   K-SHIFT KEY-BIND ;

: KEY-SLOT ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   K-SLOT KEY-BIND ;

: KEY-FRAME ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   K-FRAME KEY-BIND ;

: KEY-DSLOT ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   K-DSLOT KEY-BIND ;

: KEY-DBYTES ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   K-DBYTES KEY-BIND ;

: KEY-DBACK ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   K-DBACK KEY-BIND ;

: KEY-ENTRY ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   K-ENTRY KEY-BIND ;

\ Its own key, because two passes recognise a tail branch by its ATTRIBUTES and
\ a trap under `x64.entry` would BE one to both of them.
: KEY-TRAP-ENTRY ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   K-TRAP-ENTRY KEY-BIND ;

\ An ordinal and not an address: there is no address until the emitter has laid
\ the emission out. How many functions there are is the emitter's fact.
: KEY-FUN ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   K-FUN KEY-BIND ;

: KEY-COND ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   K-COND KEY-BIND ;

\ The literal's own value is NOT screened: `mov r64, imm64` holds every cell.
: IMM-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   IR-BUILD:INTERN-INT-ATTR ;

: IMM32-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   IMM32 IR-BUILD:INTERN-INT-ATTR ;

: ADDR-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   ADDR-KIND IR-BUILD:INTERN-INT-ATTR ;

: SHIFT-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   SHIFT-COUNT IR-BUILD:INTERN-INT-ATTR ;

: SLOT-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   SLOT IR-BUILD:INTERN-INT-ATTR ;

: FRAME-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   FRAME IR-BUILD:INTERN-INT-ATTR ;

: DSLOT-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   DSLOT IR-BUILD:INTERN-INT-ATTR ;

: DBYTES-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   DBYTES IR-BUILD:INTERN-INT-ATTR ;

: DBACK-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   DBYTES IR-BUILD:INTERN-INT-ATTR ;

: ENTRY-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   ENTRY IR-BUILD:INTERN-INT-ATTR ;

: FUN-ATTR ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-attr-id )
   FUN-ORD IR-BUILD:INTERN-INT-ATTR ;

: COND-ATTR ( IR-CTX:ctx IR-BUILD:builder X64IR:cond -- IR-ID:ir-attr-id )
   COND-CODE COND IR-BUILD:INTERN-INT-ATTR ;

private

\ ---- the schema definitions --------------------------------------------------
: RULE ( IR-CTX:ctx IR-BUILD:builder X64IR:opcode -- IR-ID:ir-symbol-id )
   MATCH opcode
      movi     OF s" x64.rule.movi"     ENDOF
      mov      OF s" x64.rule.mov"      ENDOF
      add      OF s" x64.rule.add"      ENDOF
      sub      OF s" x64.rule.sub"      ENDOF
      and      OF s" x64.rule.and"      ENDOF
      or       OF s" x64.rule.or"       ENDOF
      xor      OF s" x64.rule.xor"      ENDOF
      imul     OF s" x64.rule.imul"     ENDOF
      addi     OF s" x64.rule.addi"     ENDOF
      subi     OF s" x64.rule.subi"     ENDOF
      andi     OF s" x64.rule.andi"     ENDOF
      ori      OF s" x64.rule.ori"      ENDOF
      xori     OF s" x64.rule.xori"     ENDOF
      shli     OF s" x64.rule.shli"     ENDOF
      shri     OF s" x64.rule.shri"     ENDOF
      shl      OF s" x64.rule.shl"      ENDOF
      shr      OF s" x64.rule.shr"      ENDOF
      neg      OF s" x64.rule.neg"      ENDOF
      not      OF s" x64.rule.not"      ENDOF
      idiv     OF s" x64.rule.idiv"     ENDOF
      cmpset   OF s" x64.rule.cmpset"   ENDOF
      cmpseti  OF s" x64.rule.cmpseti"  ENDOF
      cmpsel   OF s" x64.rule.cmpsel"   ENDOF
      selz     OF s" x64.rule.selz"     ENDOF
      br       OF s" x64.rule.br"       ENDOF
      brz      OF s" x64.rule.brz"      ENDOF
      cmpbr    OF s" x64.rule.cmpbr"    ENDOF
      cmpbri   OF s" x64.rule.cmpbri"   ENDOF
      ret      OF s" x64.rule.ret"      ENDOF
      reserve  OF s" x64.rule.reserve"  ENDOF
      release  OF s" x64.rule.release"  ENDOF
      store    OF s" x64.rule.store"    ENDOF
      load     OF s" x64.rule.load"     ENDOF
      dtake    OF s" x64.rule.dtake"    ENDOF
      dload    OF s" x64.rule.dload"    ENDOF
      dstore   OF s" x64.rule.dstore"   ENDOF
      dpublish OF s" x64.rule.dpublish" ENDOF
      aload    OF s" x64.rule.aload"    ENDOF
      astore   OF s" x64.rule.astore"   ENDOF
      abload   OF s" x64.rule.abload"   ENDOF
      abstore  OF s" x64.rule.abstore"  ENDOF
      call     OF s" x64.rule.call"     ENDOF
      wordcall OF s" x64.rule.wordcall" ENDOF
      tailcall OF s" x64.rule.tailcall" ENDOF
      trap     OF s" x64.rule.trap"     ENDOF
      codeaddr OF s" x64.rule.codeaddr" ENDOF
   ;MATCH
   IR-BUILD:INTERN-SYMBOL ;

: RENDERER ( IR-CTX:ctx IR-BUILD:builder X64IR:opcode -- IR-ID:ir-symbol-id )
   MATCH opcode
      movi     OF s" x64.render.movi"     ENDOF
      mov      OF s" x64.render.mov"      ENDOF
      add      OF s" x64.render.add"      ENDOF
      sub      OF s" x64.render.sub"      ENDOF
      and      OF s" x64.render.and"      ENDOF
      or       OF s" x64.render.or"       ENDOF
      xor      OF s" x64.render.xor"      ENDOF
      imul     OF s" x64.render.imul"     ENDOF
      addi     OF s" x64.render.addi"     ENDOF
      subi     OF s" x64.render.subi"     ENDOF
      andi     OF s" x64.render.andi"     ENDOF
      ori      OF s" x64.render.ori"      ENDOF
      xori     OF s" x64.render.xori"     ENDOF
      shli     OF s" x64.render.shli"     ENDOF
      shri     OF s" x64.render.shri"     ENDOF
      shl      OF s" x64.render.shl"      ENDOF
      shr      OF s" x64.render.shr"      ENDOF
      neg      OF s" x64.render.neg"      ENDOF
      not      OF s" x64.render.not"      ENDOF
      idiv     OF s" x64.render.idiv"     ENDOF
      cmpset   OF s" x64.render.cmpset"   ENDOF
      cmpseti  OF s" x64.render.cmpseti"  ENDOF
      cmpsel   OF s" x64.render.cmpsel"   ENDOF
      selz     OF s" x64.render.selz"     ENDOF
      br       OF s" x64.render.br"       ENDOF
      brz      OF s" x64.render.brz"      ENDOF
      cmpbr    OF s" x64.render.cmpbr"    ENDOF
      cmpbri   OF s" x64.render.cmpbri"   ENDOF
      ret      OF s" x64.render.ret"      ENDOF
      reserve  OF s" x64.render.reserve"  ENDOF
      release  OF s" x64.render.release"  ENDOF
      store    OF s" x64.render.store"    ENDOF
      load     OF s" x64.render.load"     ENDOF
      dtake    OF s" x64.render.dtake"    ENDOF
      dload    OF s" x64.render.dload"    ENDOF
      dstore   OF s" x64.render.dstore"   ENDOF
      dpublish OF s" x64.render.dpublish" ENDOF
      aload    OF s" x64.render.aload"    ENDOF
      astore   OF s" x64.render.astore"   ENDOF
      abload   OF s" x64.render.abload"   ENDOF
      abstore  OF s" x64.render.abstore"  ENDOF
      call     OF s" x64.render.call"     ENDOF
      wordcall OF s" x64.render.wordcall" ENDOF
      tailcall OF s" x64.render.tailcall" ENDOF
      trap     OF s" x64.render.trap"     ENDOF
      codeaddr OF s" x64.render.codeaddr" ENDOF
   ;MATCH
   IR-BUILD:INTERN-SYMBOL ;

: NAMED ( IR-CTX:ctx IR-BUILD:builder X64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder o:X64IR:opcode :}
   c b o RULE IR-SCHEMA:SET-RULE
   c b o RENDERER IR-SCHEMA:SET-RENDERER ;

\ ---- the literal and the copy ------------------------------------------------
\ The whole cell rides in the instruction, and `x64.addr` says whether the cell
\ is a datum or an address a relocation pass has to find again. Ten bytes with
\ the imm64 at X64ASM:MOV-RI64-IMM-OFF; the site kind is SNAP-RELOC:MOVABS.
: DEF-MOVI ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b X64IR-OPCODE:MOVI OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-RESULT
   c b KEY-IMM IR-SCHEMA:ADD-ATTR
   c b KEY-ADDR IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   TARGET
   c b X64IR-OPCODE:MOVI NAMED
   c b IR-BUILD:DEFINE-OP ;

\ The form exists so that the two registers CAN be different. A copy whose ends
\ coalesce is a no-op, and whether to elide it is the allocator's decision.
: DEF-MOV ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b X64IR-OPCODE:MOV OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   TOTAL
   TARGET
   c b X64IR-OPCODE:MOV NAMED
   c b IR-BUILD:DEFINE-OP ;

\ ---- the two-address arithmetic ----------------------------------------------
\ Operand 0 is the value the instruction OVERWRITES and the result is tied to
\ it; operand 1 is the one it reads. `sub` and `imul` are not commutative in the
\ operand order, so a rewriter may only exchange the two operands of add, and,
\ or and xor, and only where it exchanges the tie with them.
: DEF-BINARY ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id X64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id o:X64IR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   0 0 IR-SCHEMA:ADD-TIE
   PURE-VALUE
   TOTAL
   TARGET
   c b o NAMED
   c b IR-BUILD:DEFINE-OP ;

\ The immediate is an ATTRIBUTE and not an operand, which is why the form costs
\ no second register. It is the signed thirty-two bits the machine sign-extends.
: DEF-BINARY-IMM ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id X64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id o:X64IR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   0 0 IR-SCHEMA:ADD-TIE
   c b KEY-IMM IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   TARGET
   c b o NAMED
   c b IR-BUILD:DEFINE-OP ;

\ The count is six bits of the imm8 the form carries. `x64.shri` is the LOGICAL
\ shift, because the engine's `rshift` is logical; an arithmetic shift is a form
\ this dialect has not got and a selector that needs one asks for it.
: DEF-SHIFT-IMM ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id X64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id o:X64IR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   0 0 IR-SCHEMA:ADD-TIE
   c b KEY-SHIFT IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   TARGET
   c b o NAMED
   c b IR-BUILD:DEFINE-OP ;

\ THE COUNT IS IN ONE REGISTER AND THE MACHINE NAMES IT. `shl r64, cl` reads the
\ count from rcx and nowhere else, so operand 1 of this form must be assigned
\ register 1. That obligation is the selector's to satisfy and the emitter's to
\ refuse; the dialect states it here because the schema has no way to declare a
\ fixed register on an operand today, and a form whose constraint is written
\ only in a pass is a constraint the next pass does not know.
: DEF-SHIFT-CL ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id X64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id o:X64IR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   0 0 IR-SCHEMA:ADD-TIE
   PURE-VALUE
   TOTAL
   TARGET
   c b o NAMED
   c b IR-BUILD:DEFINE-OP ;

: DEF-UNARY ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id X64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id o:X64IR:opcode :}
   c b o OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   0 0 IR-SCHEMA:ADD-TIE
   PURE-VALUE
   TOTAL
   TARGET
   c b o NAMED
   c b IR-BUILD:DEFINE-OP ;

\ TWO RESULTS, because one instruction leaves both: `idiv r64` divides rdx:rax
\ and writes the quotient to rax and the remainder to rdx, which is why `mod`
\ costs no second division on this machine where it costs one on ARM64. It is
\ three instructions - the dividend widened with cqo, the divide, and the branch
\ over the trap the engine's `/` carries - and it may raise, because the machine
\ raises #DE on a zero divisor and on MIN-INT / -1.
\
\ The dividend must be in rax and the remainder comes back in rdx: the same
\ fixed-register obligation DEF-SHIFT-CL states, for the same reason.
: DEF-IDIV ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b X64IR-OPCODE:IDIV OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   t IR-SCHEMA:ADD-RESULT
   PURE-VALUE
   true IR-SCHEMA:SET-TRAP
   TARGET
   c b X64IR-OPCODE:IDIV NAMED
   c b IR-BUILD:DEFINE-OP ;

\ ---- the fused comparisons ---------------------------------------------------
\ ONE operation and three instructions - compare, set one byte on the condition,
\ widen it - because the flags between them are a single architectural resource
\ no value stands for and the allocator may not hand out.
: DEF-CMPSET ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b X64IR-OPCODE:CMPSET OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   c b KEY-COND IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   TARGET
   c b X64IR-OPCODE:CMPSET NAMED
   c b IR-BUILD:DEFINE-OP ;

\ The operand is the LEFT-hand side and the immediate the right, so a rewriter
\ may fold only the second operand of a comparison.
: DEF-CMPSETI ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b X64IR-OPCODE:CMPSETI OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   c b KEY-COND IR-SCHEMA:ADD-ATTR
   c b KEY-IMM IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   TARGET
   c b X64IR-OPCODE:CMPSETI NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Compare and conditionally move: operands 0 and 1 are compared, operand 2 is
\ the value the result keeps when the condition fails and operand 3 the one
\ moved in when it holds. `cmovcc` overwrites its destination, so the result is
\ tied to operand 2 and not to operand 3.
: DEF-CMPSEL ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b X64IR-OPCODE:CMPSEL OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   0 2 IR-SCHEMA:ADD-TIE
   c b KEY-COND IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   TARGET
   c b X64IR-OPCODE:CMPSEL NAMED
   c b IR-BUILD:DEFINE-OP ;

\ The same shape with the comparison against zero the machine makes in one
\ instruction: `test rv, rv` then `cmovz`.
: DEF-SELZ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b X64IR-OPCODE:SELZ OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   0 1 IR-SCHEMA:ADD-TIE
   PURE-VALUE
   TOTAL
   TARGET
   c b X64IR-OPCODE:SELZ NAMED
   c b IR-BUILD:DEFINE-OP ;

\ ---- the branch forms --------------------------------------------------------
: DEF-BR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b X64IR-OPCODE:BR OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND-TAIL
   true 1 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   TOTAL
   TARGET
   c b X64IR-OPCODE:BR NAMED
   c b IR-BUILD:DEFINE-OP ;

\ `test rv, rv` and a jump on the zero flag: the machine has no
\ compare-and-branch-on-zero instruction, but it has a one-instruction way to
\ set the flag from a register, so this stays one operation.
: DEF-BRZ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b X64IR-OPCODE:BRZ OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   true 2 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   TOTAL
   TARGET
   c b X64IR-OPCODE:BRZ NAMED
   c b IR-BUILD:DEFINE-OP ;

\ The first successor is the CONDITION-HOLDS one, the order a64.cmpbr measured
\ and this dialect keeps so a target-free pass reads both dialects the same way.
\ Neither successor may take arguments; it defines no value.
: DEF-CMPBR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b X64IR-OPCODE:CMPBR OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   c b KEY-COND IR-SCHEMA:ADD-ATTR
   true 2 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   TOTAL
   TARGET
   c b X64IR-OPCODE:CMPBR NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Every sentence of x64.cmpbr carries over; the operand it keeps is the LEFT-hand
\ side, because the machine subtracts the immediate FROM the register.
: DEF-CMPBRI ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b X64IR-OPCODE:CMPBRI OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   c b KEY-COND IR-SCHEMA:ADD-ATTR
   c b KEY-IMM IR-SCHEMA:ADD-ATTR
   true 2 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   TOTAL
   TARGET
   c b X64IR-OPCODE:CMPBRI NAMED
   c b IR-BUILD:DEFINE-OP ;

\ `ret` pops the return address the call pushed. There is no link register to
\ restore first, which is the whole of the difference from a64.ret.
: DEF-RET ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b X64IR-OPCODE:RET OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND-TAIL
   true 0 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   TOTAL
   TARGET
   c b X64IR-OPCODE:RET NAMED
   c b IR-BUILD:DEFINE-OP ;

\ ---- the frame forms ---------------------------------------------------------
: FRAME-MEM ( IR-SCHEMA:effect -- )
   {: e:IR-SCHEMA:effect :}
   false 0 0 IR-SCHEMA:SET-CONTROL
   IR--TYPE-SPACE:LOCAL IR--SCHEMA-ALIAS:UNALIASED e IR-SCHEMA:SET-MEMORY ;

: DEF-RESERVE ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-type-id :}
   c b X64IR-OPCODE:RESERVE OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-RESULT
   c b KEY-FRAME IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:WRITE FRAME-MEM
   TOTAL
   TARGET
   c b X64IR-OPCODE:RESERVE NAMED
   c b IR-BUILD:DEFINE-OP ;

: DEF-RELEASE ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-type-id :}
   c b X64IR-OPCODE:RELEASE OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-OPERAND
   c b KEY-FRAME IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:WRITE FRAME-MEM
   TOTAL
   TARGET
   c b X64IR-OPCODE:RELEASE NAMED
   c b IR-BUILD:DEFINE-OP ;

: DEF-STORE ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id k:IR-ID:ir-type-id :}
   c b X64IR-OPCODE:STORE OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-RESULT
   c b KEY-SLOT IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:WRITE FRAME-MEM
   TOTAL
   TARGET
   c b X64IR-OPCODE:STORE NAMED
   c b IR-BUILD:DEFINE-OP ;

: DEF-LOAD ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id k:IR-ID:ir-type-id :}
   c b X64IR-OPCODE:LOAD OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   k IR-SCHEMA:ADD-RESULT
   c b KEY-SLOT IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:READ FRAME-MEM
   TOTAL
   TARGET
   c b X64IR-OPCODE:LOAD NAMED
   c b IR-BUILD:DEFINE-OP ;

\ ---- the data-stack forms ----------------------------------------------------
\ Unrestricted aliasing is an ORDERING statement: it forbids moving one access
\ across another it cannot prove is elsewhere, and nothing more than that.
: DSTACK-MEM ( IR-SCHEMA:effect -- )
   {: e:IR-SCHEMA:effect :}
   false 0 0 IR-SCHEMA:SET-CONTROL
   IR--TYPE-SPACE:GENERIC IR--SCHEMA-ALIAS:UNRESTRICTED e IR-SCHEMA:SET-MEMORY ;

: DSTACK-TERM-MEM ( IR-SCHEMA:effect -- )
   {: e:IR-SCHEMA:effect :}
   true 0 0 IR-SCHEMA:SET-CONTROL
   IR--TYPE-SPACE:GENERIC IR--SCHEMA-ALIAS:UNRESTRICTED e IR-SCHEMA:SET-MEMORY ;

: DEF-DTAKE ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-type-id :}
   c b X64IR-OPCODE:DTAKE OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-RESULT
   c b KEY-DBYTES IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:WRITE DSTACK-MEM
   TOTAL
   TARGET
   c b X64IR-OPCODE:DTAKE NAMED
   c b IR-BUILD:DEFINE-OP ;

: DEF-DLOAD ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id k:IR-ID:ir-type-id :}
   c b X64IR-OPCODE:DLOAD OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   k IR-SCHEMA:ADD-RESULT
   c b KEY-DSLOT IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:READ DSTACK-MEM
   TOTAL
   TARGET
   c b X64IR-OPCODE:DLOAD NAMED
   c b IR-BUILD:DEFINE-OP ;

: DEF-DSTORE ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id k:IR-ID:ir-type-id :}
   c b X64IR-OPCODE:DSTORE OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-RESULT
   c b KEY-DSLOT IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:WRITE DSTACK-MEM
   TOTAL
   TARGET
   c b X64IR-OPCODE:DSTORE NAMED
   c b IR-BUILD:DEFINE-OP ;

\ The pointer move that publishes what the stores above wrote: `add r12, imm32`
\ or `sub`, which is one instruction either way.
: DEF-DPUBLISH ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-type-id :}
   c b X64IR-OPCODE:DPUBLISH OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-OPERAND
   c b KEY-DBYTES IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:WRITE DSTACK-MEM
   TOTAL
   TARGET
   c b X64IR-OPCODE:DPUBLISH NAMED
   c b IR-BUILD:DEFINE-OP ;

\ ---- the addressed forms -----------------------------------------------------
: ADDR-MEM ( IR-SCHEMA:effect -- )
   {: e:IR-SCHEMA:effect :}
   false 0 0 IR-SCHEMA:SET-CONTROL
   IR--TYPE-SPACE:GENERIC IR--SCHEMA-ALIAS:UNRESTRICTED e IR-SCHEMA:SET-MEMORY ;

\ There is no offset attribute: the form encodes at displacement zero.
: DEF-ALOAD ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id k:IR-ID:ir-type-id :}
   c b X64IR-OPCODE:ALOAD OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   k IR-SCHEMA:ADD-RESULT
   IR--SCHEMA-EFFECT:READ ADDR-MEM
   TOTAL
   TARGET
   c b X64IR-OPCODE:ALOAD NAMED
   c b IR-BUILD:DEFINE-OP ;

: DEF-ASTORE ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id k:IR-ID:ir-type-id :}
   c b X64IR-OPCODE:ASTORE OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-RESULT
   IR--SCHEMA-EFFECT:WRITE ADDR-MEM
   TOTAL
   TARGET
   c b X64IR-OPCODE:ASTORE NAMED
   c b IR-BUILD:DEFINE-OP ;

\ The width is the FORM: `movzx r64, byte [r]` loads the byte zero-extended,
\ which is what `c@` leaves, and `mov byte [r], r8` stores the low byte.
: DEF-ABLOAD ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id k:IR-ID:ir-type-id :}
   c b X64IR-OPCODE:ABLOAD OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-RESULT
   k IR-SCHEMA:ADD-RESULT
   IR--SCHEMA-EFFECT:READ ADDR-MEM
   TOTAL
   TARGET
   c b X64IR-OPCODE:ABLOAD NAMED
   c b IR-BUILD:DEFINE-OP ;

: DEF-ABSTORE ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id k:IR-ID:ir-type-id :}
   c b X64IR-OPCODE:ABSTORE OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-OPERAND
   t IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-RESULT
   IR--SCHEMA-EFFECT:WRITE ADDR-MEM
   TOTAL
   TARGET
   c b X64IR-OPCODE:ABSTORE NAMED
   c b IR-BUILD:DEFINE-OP ;

\ ---- the calls ---------------------------------------------------------------
\ `call rel32` pushes the return address and costs the routine nothing else: the
\ eight bytes it pushes are the whole of this machine's call frame, where ARM64
\ spends a register and a frame slot on x30.
: DEF-CALL ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-type-id :}
   c b X64IR-OPCODE:CALL OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-RESULT
   c b KEY-DBYTES IR-SCHEMA:ADD-ATTR
   c b KEY-DBACK IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:READ-WRITE DSTACK-MEM
   true IR-SCHEMA:SET-TRAP
   TARGET
   c b X64IR-OPCODE:CALL NAMED
   c b IR-BUILD:DEFINE-OP ;

: DEF-WORDCALL ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-type-id :}
   c b X64IR-OPCODE:WORDCALL OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-OPERAND
   k IR-SCHEMA:ADD-RESULT
   c b KEY-DBYTES IR-SCHEMA:ADD-ATTR
   c b KEY-DBACK IR-SCHEMA:ADD-ATTR
   c b KEY-ENTRY IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:READ-WRITE DSTACK-MEM
   true IR-SCHEMA:SET-TRAP
   TARGET
   c b X64IR-OPCODE:WORDCALL NAMED
   c b IR-BUILD:DEFINE-OP ;

\ A TERMINATOR and not a call: `jmp rel32` to the callee, which returns to this
\ routine's own caller. It carries NO adjustment, which is what makes it one
\ instruction: the selector only chooses it where the pointer already stands at
\ the callee's entry base.
: DEF-TAILCALL ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-type-id :}
   c b X64IR-OPCODE:TAILCALL OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-OPERAND
   c b KEY-ENTRY IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:WRITE DSTACK-TERM-MEM
   true IR-SCHEMA:SET-TRAP
   TARGET
   c b X64IR-OPCODE:TAILCALL NAMED
   c b IR-BUILD:DEFINE-OP ;

\ Its own form and not the tail branch with another target: a tail branch is how
\ a routine RETURNS, and this publishes nothing and comes back from nowhere.
: DEF-TRAP ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:IR-ID:ir-type-id :}
   c b X64IR-OPCODE:TRAP OPCODE IR-SCHEMA:BEGIN-OP
   k IR-SCHEMA:ADD-OPERAND
   c b KEY-TRAP-ENTRY IR-SCHEMA:ADD-ATTR
   c b KEY-DBYTES IR-SCHEMA:ADD-ATTR
   IR--SCHEMA-EFFECT:WRITE DSTACK-TERM-MEM
   true IR-SCHEMA:SET-TRAP
   TARGET
   c b X64IR-OPCODE:TRAP NAMED
   c b IR-BUILD:DEFINE-OP ;

\ The address of one of this emission's own functions, in the same ten bytes the
\ literal uses, so a quotation's address is one relocation site like any other.
: DEF-CODEADDR ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-type-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-type-id :}
   c b X64IR-OPCODE:CODEADDR OPCODE IR-SCHEMA:BEGIN-OP
   t IR-SCHEMA:ADD-RESULT
   c b KEY-FUN IR-SCHEMA:ADD-ATTR
   PURE-VALUE
   TOTAL
   TARGET
   c b X64IR-OPCODE:CODEADDR NAMED
   c b IR-BUILD:DEFINE-OP ;

public

\ ---- the table this dialect may fill -----------------------------------------
\ The table's dialect name and version are fixed when the module is created, so
\ reading them back off the live module decides whose table it is.
: DIALECT-CK ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b IR-BUILD:DIALECT@  NAME IR-BUILD:SYMBOL-IS?
   0= if E-X64IR-DIALECT throw then
   c b IR-BUILD:SCHEMA-MAJOR@ MAJOR <> if E-X64IR-DIALECT throw then
   c b IR-BUILD:SCHEMA-MINOR@ MINOR <> if E-X64IR-DIALECT throw then ;

private

\ ---- the session prototype ---------------------------------------------------
\ Every module a definition builds would intern this dialect's whole vocabulary
\ again, because a module's symbols are its own ordinals. A LOAD interns it once
\ instead, into an interner of the session's own, and every module built while
\ that prototype stands starts as a copy of it.
2 TYPED-BUFFER PROTO IR-ARENA:arena
variable PROTO-ON
0 PROTO-ON !

: PRE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key ptr u8 n -- IR-ID:ir-symbol-id )
   IR-SYM:INTERN ;

: PRE-OP ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena k:IR-ID:ir-module-key i:n :}
   c a r k i NTH OP-NAME PRE IR-ID:SYMBOL-LOCAL i PROTO-ORD ! ;

: PRE-KEY ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena k:IR-ID:ir-module-key i:n :}
   c a r k i KEY-NAME PRE IR-ID:SYMBOL-LOCAL  OPCODES i + PROTO-ORD ! ;

public

\ Intern this dialect's whole vocabulary into a session-lived interner and keep
\ it. Walked, not listed, so the prototype cannot fall behind the tables above.
: PROTOTYPE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena k:IR-ID:ir-module-key :}
   c a r k NAME PRE drop
   OPCODES 0 ?do c a r k i PRE-OP loop
   KEYS 0 ?do c a r k i PRE-KEY loop
   a 0 PROTO !
   r 1 PROTO !
   1 PROTO-ON ! ;

: PROTOTYPE-CLEAR ( -- )
   0 PROTO-ON !
   MEMO-FORGET ;

\ ---- creation and registration -----------------------------------------------
\ While a session prototype stands, the module's interner starts as a copy of
\ it; the module identity, the plan and every check are the ordinary ones.
: NEW-BUILDER ( IR-CTX:ctx -- IR-BUILD:builder )
   dup CHECK-TARGET
   PROTO-ON @ 0= if NAME MAJOR MINOR IR-BUILD:NEW-BUILDER exit then
   NAME MAJOR MINOR 0 PROTO @ 1 PROTO @ IR-BUILD:NEW-BUILDER-FROM
   dup MEMO-ADOPT ;

\ Definition is one opcode at a time, so a refusal leaves the opcodes already
\ defined and defines no more.
: REGISTER ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b DIALECT-CK
   c b GPR-TYPE {: t:IR-ID:ir-type-id :}
   c b MEM-TYPE {: k:IR-ID:ir-type-id :}
   c b t DEF-MOVI
   c b t DEF-MOV
   c b t X64IR-OPCODE:ADD DEF-BINARY
   c b t X64IR-OPCODE:SUB DEF-BINARY
   c b t X64IR-OPCODE:AND DEF-BINARY
   c b t X64IR-OPCODE:OR DEF-BINARY
   c b t X64IR-OPCODE:XOR DEF-BINARY
   c b t X64IR-OPCODE:IMUL DEF-BINARY
   c b t X64IR-OPCODE:ADDI DEF-BINARY-IMM
   c b t X64IR-OPCODE:SUBI DEF-BINARY-IMM
   c b t X64IR-OPCODE:ANDI DEF-BINARY-IMM
   c b t X64IR-OPCODE:ORI DEF-BINARY-IMM
   c b t X64IR-OPCODE:XORI DEF-BINARY-IMM
   c b t X64IR-OPCODE:SHLI DEF-SHIFT-IMM
   c b t X64IR-OPCODE:SHRI DEF-SHIFT-IMM
   c b t X64IR-OPCODE:SHL DEF-SHIFT-CL
   c b t X64IR-OPCODE:SHR DEF-SHIFT-CL
   c b t X64IR-OPCODE:NEG DEF-UNARY
   c b t X64IR-OPCODE:NOT DEF-UNARY
   c b t DEF-IDIV
   c b t DEF-CMPSET
   c b t DEF-CMPSETI
   c b t DEF-CMPSEL
   c b t DEF-SELZ
   c b t DEF-BR
   c b t DEF-BRZ
   c b t DEF-CMPBR
   c b t DEF-CMPBRI
   c b t DEF-RET
   c b k DEF-RESERVE
   c b k DEF-RELEASE
   c b t k DEF-STORE
   c b t k DEF-LOAD
   c b k DEF-DTAKE
   c b t k DEF-DLOAD
   c b t k DEF-DSTORE
   c b k DEF-DPUBLISH
   c b t k DEF-ALOAD
   c b t k DEF-ASTORE
   c b t k DEF-ABLOAD
   c b t k DEF-ABSTORE
   c b k DEF-CALL
   c b k DEF-WORDCALL
   c b k DEF-TAILCALL
   c b k DEF-TRAP
   c b t DEF-CODEADDR ;

private

: DEFINE-ONE ( IR-CTX:ctx IR-BUILD:builder X64IR:opcode -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder o:X64IR:opcode :}
   o MATCH opcode
      movi     OF c b c b GPR-TYPE DEF-MOVI ENDOF
      mov      OF c b c b GPR-TYPE DEF-MOV ENDOF
      add      OF c b c b GPR-TYPE X64IR-OPCODE:ADD DEF-BINARY ENDOF
      sub      OF c b c b GPR-TYPE X64IR-OPCODE:SUB DEF-BINARY ENDOF
      and      OF c b c b GPR-TYPE X64IR-OPCODE:AND DEF-BINARY ENDOF
      or       OF c b c b GPR-TYPE X64IR-OPCODE:OR DEF-BINARY ENDOF
      xor      OF c b c b GPR-TYPE X64IR-OPCODE:XOR DEF-BINARY ENDOF
      imul     OF c b c b GPR-TYPE X64IR-OPCODE:IMUL DEF-BINARY ENDOF
      addi     OF c b c b GPR-TYPE X64IR-OPCODE:ADDI DEF-BINARY-IMM ENDOF
      subi     OF c b c b GPR-TYPE X64IR-OPCODE:SUBI DEF-BINARY-IMM ENDOF
      andi     OF c b c b GPR-TYPE X64IR-OPCODE:ANDI DEF-BINARY-IMM ENDOF
      ori      OF c b c b GPR-TYPE X64IR-OPCODE:ORI DEF-BINARY-IMM ENDOF
      xori     OF c b c b GPR-TYPE X64IR-OPCODE:XORI DEF-BINARY-IMM ENDOF
      shli     OF c b c b GPR-TYPE X64IR-OPCODE:SHLI DEF-SHIFT-IMM ENDOF
      shri     OF c b c b GPR-TYPE X64IR-OPCODE:SHRI DEF-SHIFT-IMM ENDOF
      shl      OF c b c b GPR-TYPE X64IR-OPCODE:SHL DEF-SHIFT-CL ENDOF
      shr      OF c b c b GPR-TYPE X64IR-OPCODE:SHR DEF-SHIFT-CL ENDOF
      neg      OF c b c b GPR-TYPE X64IR-OPCODE:NEG DEF-UNARY ENDOF
      not      OF c b c b GPR-TYPE X64IR-OPCODE:NOT DEF-UNARY ENDOF
      idiv     OF c b c b GPR-TYPE DEF-IDIV ENDOF
      cmpset   OF c b c b GPR-TYPE DEF-CMPSET ENDOF
      cmpseti  OF c b c b GPR-TYPE DEF-CMPSETI ENDOF
      cmpsel   OF c b c b GPR-TYPE DEF-CMPSEL ENDOF
      selz     OF c b c b GPR-TYPE DEF-SELZ ENDOF
      br       OF c b c b GPR-TYPE DEF-BR ENDOF
      brz      OF c b c b GPR-TYPE DEF-BRZ ENDOF
      cmpbr    OF c b c b GPR-TYPE DEF-CMPBR ENDOF
      cmpbri   OF c b c b GPR-TYPE DEF-CMPBRI ENDOF
      ret      OF c b c b GPR-TYPE DEF-RET ENDOF
      reserve  OF c b c b MEM-TYPE DEF-RESERVE ENDOF
      release  OF c b c b MEM-TYPE DEF-RELEASE ENDOF
      store    OF c b c b GPR-TYPE c b MEM-TYPE DEF-STORE ENDOF
      load     OF c b c b GPR-TYPE c b MEM-TYPE DEF-LOAD ENDOF
      dtake    OF c b c b MEM-TYPE DEF-DTAKE ENDOF
      dload    OF c b c b GPR-TYPE c b MEM-TYPE DEF-DLOAD ENDOF
      dstore   OF c b c b GPR-TYPE c b MEM-TYPE DEF-DSTORE ENDOF
      dpublish OF c b c b MEM-TYPE DEF-DPUBLISH ENDOF
      aload    OF c b c b GPR-TYPE c b MEM-TYPE DEF-ALOAD ENDOF
      astore   OF c b c b GPR-TYPE c b MEM-TYPE DEF-ASTORE ENDOF
      abload   OF c b c b GPR-TYPE c b MEM-TYPE DEF-ABLOAD ENDOF
      abstore  OF c b c b GPR-TYPE c b MEM-TYPE DEF-ABSTORE ENDOF
      call     OF c b c b MEM-TYPE DEF-CALL ENDOF
      wordcall OF c b c b MEM-TYPE DEF-WORDCALL ENDOF
      tailcall OF c b c b MEM-TYPE DEF-TAILCALL ENDOF
      trap     OF c b c b MEM-TYPE DEF-TRAP ENDOF
      codeaddr OF c b c b GPR-TYPE DEF-CODEADDR ENDOF
   ;MATCH ;

public

\ Materialize only a requested opcode, retaining the module's schema as the sole
\ presence authority. OPCODE and BIND remain pure symbol interning.
: ENSURE-OP ( IR-CTX:ctx IR-BUILD:builder X64IR:opcode -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder o:X64IR:opcode :}
   c b DIALECT-CK
   c b o ORD MEMO-BIND {: op:IR-ID:ir-symbol-id :}
   c b op IR-BUILD:SCHEMA-DEFINED? 0= if c b o DEFINE-ONE then
   op ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package

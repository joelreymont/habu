\ x64ir.f - checked x86-64 machine dialect tests.
\
\ Proves the contract of src/compiler/native/x64ir.f: loading the backend puts
\ the x86-64 row in the compiler's target registry and both stages accept the
\ machine this dialect builds for; registering the dialect defines the machine
\ opcodes and their declared fields read back through the frozen schema table,
\ with the two-address forms declaring the tie that says WHICH operand the
\ instruction overwrites; the register file is the machine's, with the stack
\ pointer among the registers no allocation may hand out; the conditions are the
\ signed ones the shipped assembler spells; and an immediate, a shift count, a
\ frame slot, a reserved frame, a data-stack offset, an entry address or a
\ function ordinal outside its field is refused before it can be interned as an
\ attribute.
\
\ WHY THE TIES ARE THE CENTRE OF THIS SUITE. On ARM64 a tie is an oddity of the
\ move-wide overwrite; here it is how nearly every arithmetic form works, and a
\ tie that named the wrong operand would compile into code that destroys a live
\ value with no refusal anywhere. Each tied form is therefore read back by
\ ordinal - the result AND the operand it names - and not merely counted.
\
\ WHY THE STACK POINTER IS ASSERTED RESERVED. docs/x86-64.md lists six virtual
\ machine registers (rbp, rbx, r12..r15) and rsp is not among them, because
\ ARM64's stack pointer is not one of the registers an operand field can name
\ and the transcription carried that over. On x86-64 it is register 4: `call`
\ pushes through it, so a routine that allocated it would destroy its own
\ return. Nine registers are allocatable here and not ten.
\
\ ONE FIXTURE PER CONTEXT. A module holds about seventeen arenas and the live
\ arena registry holds sixty-four, so each group below runs in its own context.

require lib/test.f
require src/compiler/ir/symbol.f
require src/compiler/native/x64ir.f
require src/arch/x86-64/machine.f
require src/compiler/native-effect.f
require src/arch/x86-64/asm.f

package X64IR-TEST
private

\ ---- bindings ----------------------------------------------------------------
\ A linux x86-64 contract whose integer overflow traps: a machine Add does not
\ trap on this architecture either, so the trapping policy is where "no form of
\ this dialect traps except the divide" is measured.
: CTR ( -- CTARGET:contract )
   CTARGET-ARCH:X86-64 CTARGET-ABI:SYSV-AMD64 CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT ;

: BND ( -- CBIND:binding )
   CTR
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ A GPU kernel contract: this dialect is the native pipeline's, and PTX has no
\ backend loaded at all, so the registry refuses before this dialect is asked.
: PCTR ( -- CTARGET:contract )
   CTARGET-ARCH:PTX CTARGET-ABI:PTX-KERNEL CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT ;

: PBND ( -- CBIND:binding )
   PCTR
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ ---- module rigging ----------------------------------------------------------
: MOD-NEW ( IR-CTX:ctx -- IR-BUILD:builder )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   c X64IR:NEW-BUILDER ;

: DIALECT-NEW ( IR-CTX:ctx -- IR-BUILD:builder )
   {: c:IR-CTX:ctx :}
   c MOD-NEW {: b:IR-BUILD:builder :}
   c b X64IR:REGISTER
   b ;

\ ---- the registry row --------------------------------------------------------
\ src/arch/x86-64/backend.f is required by this dialect, so the row exists
\ exactly because backend code is loaded, and both stages answer for the machine
\ the dialect builds for.
: REGISTRY-CASE ( -- )
   s" loading the x86-64 backend fills its registry row" T-LABEL
   CTARGET-ARCH:X86-64 CTARGET:REGISTERED? TTRUE
   CTR CTARGET:LOWERS? TTRUE
   CTR CTARGET:EMITS? TTRUE
   s" an architecture with no backend loaded is refused by the registry" T-LABEL
   [: PCTR CTARGET:LOWERS? drop ;] E-CTGT-UNLOADED TTHROWSQ ;

\ ---- the closed opcode vocabulary --------------------------------------------
: OPCODE-NTH-LOW ( -- )   -1 X64IR:NTH drop ;
: OPCODE-NTH-HIGH ( -- )  X64IR:OPCODES X64IR:NTH drop ;

: OPCODE-ORDINAL-CASE ( -- )
   s" every machine opcode round-trips through the dialect-owned ordinal" T-LABEL
   X64IR:OPCODES 0 ?do
      i X64IR:NTH X64IR:ORD i T=
   loop
   s" machine opcode ordinals below the vocabulary are refused" T-LABEL
   [: OPCODE-NTH-LOW ;] E-X64IR-OPCODE TTHROWSQ
   s" machine opcode ordinals above the vocabulary are refused" T-LABEL
   [: OPCODE-NTH-HIGH ;] E-X64IR-OPCODE TTHROWSQ ;

\ ---- the register file -------------------------------------------------------
\ The numbers are the machine's own: rbx 3, rsp 4, rbp 5 and r12..r15 are the
\ virtual machine's, and rax, rcx, rdx, rsi, rdi and r8..r11 are what is left.
: R-MASK ( -- n )
   1 3 lshift  1 4 lshift or  1 5 lshift or
   1 12 lshift or  1 13 lshift or  1 14 lshift or  1 15 lshift or ;

: A-MASK ( -- n )   $FFFF R-MASK xor ;

: REGFILE-CASE ( -- )
   s" the register file is sixteen general registers, seven of them the VM's" T-LABEL
   X64IR:REGFILE NREGFILE:GPR-SIZE 16 T=
   X64IR:REGFILE NREGFILE:GPR-RESERVED NREGFILE:REGS-BITS R-MASK T=
   X64IR:REGFILE NREGFILE:GPR-ALLOCATABLE NREGFILE:REGS-BITS A-MASK T=
   s" nine registers are allocatable, because rsp is not one of them" T-LABEL
   X64IR:REGFILE NREGFILE:GPR-ALLOCATABLE NREGFILE:REGS-BITS $10 and 0 T=
   s" every allocatable register is destroyed by a call" T-LABEL
   X64IR:REGFILE NREGFILE:GPR-CLOBBERED NREGFILE:REGS-BITS A-MASK T=
   X64IR:REGFILE NREGFILE:GPR-CALLEE-SAVED NREGFILE:REGS-BITS 0 T=
   s" sixteen floating registers, none of them reserved" T-LABEL
   X64IR:REGFILE NREGFILE:FPR-SIZE 16 T=
   X64IR:REGFILE NREGFILE:FPR-RESERVED NREGFILE:REGS-BITS 0 T=
   X64IR:REGFILE NREGFILE:FPR-ALLOCATABLE NREGFILE:REGS-BITS $FFFF T=
   s" a frame slot is one cell wide" T-LABEL
   X64IR:REGFILE NREGFILE:SLOT-WIDTH 8 T=
   X64IR:SLOT-WIDTH 8 T= ;

\ ---- the machine, as a routine contract is written against it ----------------
\ src/arch/x86-64/machine.f is the second instance of the description the
\ routine-effect schema was generalised over, and the first machine that
\ disagrees with ARM64 about facts the schema used to hold as constants. Three
\ of those disagreements are proved here because no ARM64 case can state them:
\ there is no link register, so `absent` is the only link field a contract of
\ this machine may carry and the two ARM64 answers are refused; the file numbers
\ sixteen registers, so a register the schema can write down is still not one a
\ routine of THIS machine may hold state in; and the frame offset is counted in
\ bytes rather than scaled by the access width, so one width reaches exactly as
\ far as another.
: X-ROUTINE ( NEFF:gprs NEFF:link -- NEFF:routine )
   {: gc:NEFF:gprs l:NEFF:link :}
   NEFF-CONV:REGISTER NEFF:SEQ-NONE NEFF:SEQ-NONE gc
   NEFF:FPR-NONE NEFF:FPR-NONE NEFF:FPR-NONE
   NEFF-NZCV:UNTOUCHED l NEFF-CONTROL:RETURNS
   NEFF:TRAITS-NONE 0 0 X64M:MACHINE NEFF:ROUTINE ;

\ A contract is fourteen cells, so a case that wants only the refusal unmakes
\ what it built.
: DROP-ROUTINE ( NEFF:routine -- )
   NEFF-ROUTINE:UNMAKE
   drop drop drop drop drop drop drop drop drop drop drop drop drop drop ;

: DESTROYS ( n -- )
   NEFF:GPR-REG NEFF-LINK:ABSENT X-ROUTINE DROP-ROUTINE ;

: MACHINE-CASE ( -- )
   s" the machine a contract carries is this backend's" T-LABEL
   NEFF:GPR-NONE NEFF-LINK:ABSENT X-ROUTINE NEFF:MACH@
      X64M:MACHINE NMACH-MACH:EQ TTRUE
   s" and it is a machine with no link register" T-LABEL
   X64M:MACHINE NMACH:LINK? TFALSE
   s" so a contract of it cannot preserve or clobber one" T-LABEL
   [: NEFF:GPR-NONE NEFF-LINK:PRESERVED X-ROUTINE DROP-ROUTINE ;]
      E-NEFF-LINK TTHROWSQ
   [: NEFF:GPR-NONE NEFF-LINK:CLOBBERED X-ROUTINE DROP-ROUTINE ;]
      E-NEFF-LINK TTHROWSQ

   s" the registers a routine may hold state in are the file's nine" T-LABEL
   X64M:MACHINE NEFF:GPR-ALL NEFF:GPRS-N A-MASK T=
   X64M:MACHINE NEFF:FPR-ALL NEFF:FPRS-N $FFFF T=
   s" a register past the file is refused however the schema writes it" T-LABEL
   [: 16 DESTROYS ;] E-NEFF-GPR TTHROWSQ
   [: 31 DESTROYS ;] E-NEFF-GPR TTHROWSQ
   s" and so is the stack pointer, which here is an ordinary operand number" T-LABEL
   [: X64IR:SP-GPR DESTROYS ;] E-NEFF-GPR TTHROWSQ
   s" while a register the file does hand out is admitted" T-LABEL
   0 DESTROYS

   s" the frame bound and the alignment are the dialect's" T-LABEL
   X64M:MACHINE NMACH:FRAME-MAX X64IR:FRAME-LIMIT T=
   X64M:MACHINE NMACH:SP-ALIGN X64IR:SP-ALIGN T=
   X64M:MACHINE NMACH:SP-GPR X64IR:SP-GPR T=
   s" a displacement counted in bytes reaches the same byte at every width" T-LABEL
   1 X64M:MACHINE NMACH:SLOT-REACH X64IR:DISP-LIMIT T=
   4 X64M:MACHINE NMACH:SLOT-REACH X64IR:DISP-LIMIT T=
   8 X64M:MACHINE NMACH:SLOT-REACH X64IR:DISP-LIMIT T=
   s" a halfword access exists here, and a width no mov moves does not" T-LABEL
   2 X64M:MACHINE NMACH:WIDTH-OK? TTRUE
   3 X64M:MACHINE NMACH:WIDTH-OK? TFALSE
   [: 3 X64M:MACHINE NMACH:SLOT-REACH drop ;] E-NMACH TTHROWSQ ;

\ ---- the frame forms the spill rewriter is handed ----------------------------
\ The rewriter asks the lowering record and never the dialect, so what this
\ dialect answers is pinned by name: the general pair and the trap are its
\ own forms, and the four forms src/compiler/native/x64ir.f leaves absent are
\ absent - the floating pair because nothing this dialect allocates leaves the
\ floating file, the link pair because the return address lives on the machine
\ stack. A float slot under this record is refused by
\ src/compiler/native/spill.f STORE-FORM, which test/compiler/native-regalloc.f
\ measures on a record with the pair taken out.
: LOWERING-BODY ( IR-CTX:ctx -- bool bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b X64IR:LOWERING NDIALECT-LOWERING:UNMAKE
   {: nm:IR-ID:ir-symbol-id mj:n mi:n
      gpr:IR-ID:ir-type-id fpr:IR-ID:ir-type-id mem:IR-ID:ir-type-id
      slot:IR-ID:ir-symbol-id frame:IR-ID:ir-symbol-id
      copy:IR-ID:ir-symbol-id remat:IR-ID:ir-symbol-id
      reserve:IR-ID:ir-symbol-id release:IR-ID:ir-symbol-id
      store:IR-ID:ir-symbol-id load:IR-ID:ir-symbol-id
      fstore:NDIALECT:optsym fload:NDIALECT:optsym
      trapop:IR-ID:ir-symbol-id
      linksave:NDIALECT:optsym linkload:NDIALECT:optsym :}
   store IR-ID:SYMBOL-LOCAL
      c b X64IR-OPCODE:STORE X64IR:OPCODE IR-ID:SYMBOL-LOCAL =
   load IR-ID:SYMBOL-LOCAL
      c b X64IR-OPCODE:LOAD X64IR:OPCODE IR-ID:SYMBOL-LOCAL =
   trapop IR-ID:SYMBOL-LOCAL
      c b X64IR-OPCODE:TRAP X64IR:OPCODE IR-ID:SYMBOL-LOCAL =
   fstore NDIALECT:HAS?  fload NDIALECT:HAS?
   linksave NDIALECT:HAS?  linkload NDIALECT:HAS? ;

: LOWERING-CASE ( -- )
   s" the general frame pair and the trap are named for the spill rewriter" T-LABEL
   BND [: LOWERING-BODY ;] IR-CTX:WITH-CONTEXT
   {: st:bool ld:bool tr:bool fs:bool fl:bool ls:bool ll:bool :}
   st TTRUE ld TTRUE tr TTRUE
   s" and the floating pair and the link pair are absent" T-LABEL
   fs TFALSE fl TFALSE ls TFALSE ll TFALSE ;

\ ---- the conditions ----------------------------------------------------------
\ Each code is the shipped assembler's own word, so this case states WHICH word
\ each source relation lowers under. A comparison of Habu cells is signed, which
\ is why every one of them is an L/G condition and never a B/A one.
: COND-CASE ( -- )
   s" the six conditions are the assembler's signed ones" T-LABEL
   X64IR-COND:LT X64IR:COND-CODE X64ASM:C-L X64ASM:CONDITION>N T=
   X64IR-COND:LE X64IR:COND-CODE X64ASM:C-LE X64ASM:CONDITION>N T=
   X64IR-COND:GT X64IR:COND-CODE X64ASM:C-G X64ASM:CONDITION>N T=
   X64IR-COND:GE X64IR:COND-CODE X64ASM:C-GE X64ASM:CONDITION>N T=
   X64IR-COND:EQUAL X64IR:COND-CODE X64ASM:C-E X64ASM:CONDITION>N T=
   X64IR-COND:NE X64IR:COND-CODE X64ASM:C-NE X64ASM:CONDITION>N T=
   s" no condition is one of the unsigned pair the same relation has" T-LABEL
   X64IR-COND:LT X64IR:COND-CODE X64ASM:C-B X64ASM:CONDITION>N = TFALSE
   X64IR-COND:GT X64IR:COND-CODE X64ASM:C-A X64ASM:CONDITION>N = TFALSE
   s" a condition round-trips through its machine code" T-LABEL
   X64IR-COND:LT X64IR:COND-CODE X64IR:N>COND X64IR-COND:LT X64IR-COND:EQ TTRUE
   X64IR-COND:NE X64IR:COND-CODE X64IR:N>COND X64IR-COND:NE X64IR-COND:EQ TTRUE
   s" a code outside the four-bit field is no condition of this dialect" T-LABEL
   [: 16 X64IR:N>COND drop ;] E-X64IR-COND TTHROWSQ
   [: 2 X64IR:N>COND drop ;] E-X64IR-COND TTHROWSQ ;

\ ---- the declared shapes -----------------------------------------------------
\ The literal defines a value out of nothing and carries both of its attributes;
\ the copy reads one register and writes another with NO tie, which is what
\ makes it the form an allocator may coalesce away.
: MOVE-BODY ( IR-CTX:ctx -- n n n n n n )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b X64IR-OPCODE:MOVI X64IR:OPCODE {: i:IR-ID:ir-symbol-id :}
   c b X64IR-OPCODE:MOV X64IR:OPCODE {: v:IR-ID:ir-symbol-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   rv i IR-SCHEMA:FOPERANDS
   rv i IR-SCHEMA:FRESULTS
   rv i IR-SCHEMA:FATTRS
   rv v IR-SCHEMA:FOPERANDS
   rv v IR-SCHEMA:FRESULTS
   rv v IR-SCHEMA:FTIES ;

: MOVE-CASE ( -- )
   s" the literal carries its value and its address kind; the copy ties nothing" T-LABEL
   BND [: MOVE-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 1 T= 1 T= 2 T= 1 T= 0 T= ;

\ Every two-address form declares one tie, and the tie names the operand the
\ instruction overwrites: operand 0 for the arithmetic, operand 2 for the
\ conditional move whose destination is the value kept when the condition fails,
\ operand 1 for the select against zero.
: TIE-BODY ( IR-CTX:ctx -- n n n n n n n n n n )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b X64IR-OPCODE:ADD X64IR:OPCODE {: a:IR-ID:ir-symbol-id :}
   c b X64IR-OPCODE:SUBI X64IR:OPCODE {: s:IR-ID:ir-symbol-id :}
   c b X64IR-OPCODE:CMPSEL X64IR:OPCODE {: p:IR-ID:ir-symbol-id :}
   c b X64IR-OPCODE:SELZ X64IR:OPCODE {: z:IR-ID:ir-symbol-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FSCHEMA-POOL {: qv:IR-ARENA:view :}
   m IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   rv a IR-SCHEMA:FTIES
   qv rv a 0 IR-SCHEMA:FTIE-RESULT@
   qv rv a 0 IR-SCHEMA:FTIE-OPERAND@
   rv s IR-SCHEMA:FTIES
   qv rv s 0 IR-SCHEMA:FTIE-OPERAND@
   rv p IR-SCHEMA:FOPERANDS
   qv rv p 0 IR-SCHEMA:FTIE-OPERAND@
   rv z IR-SCHEMA:FOPERANDS
   qv rv z 0 IR-SCHEMA:FTIE-OPERAND@
   rv z IR-SCHEMA:FRESULTS ;

: TIE-CASE ( -- )
   s" each two-address form ties its result to the operand it overwrites" T-LABEL
   BND [: TIE-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 1 T= 3 T= 2 T= 4 T= 0 T= 1 T= 0 T= 0 T= 1 T= ;

\ The divide is the one form that may raise - the machine raises on a zero
\ divisor - and the one that leaves TWO results, because `idiv` writes the
\ quotient and the remainder in one instruction.
: DIVIDE-BODY ( IR-CTX:ctx -- n n bool bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b X64IR-OPCODE:IDIV X64IR:OPCODE {: d:IR-ID:ir-symbol-id :}
   c b X64IR-OPCODE:IMUL X64IR:OPCODE {: u:IR-ID:ir-symbol-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   rv d IR-SCHEMA:FOPERANDS
   rv d IR-SCHEMA:FRESULTS
   rv d IR-SCHEMA:FTRAPS?
   rv u IR-SCHEMA:FTRAPS? ;

: DIVIDE-CASE ( -- )
   s" the divide leaves two results and is the only arithmetic that may raise" T-LABEL
   BND [: DIVIDE-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TTRUE 2 T= 2 T= ;

\ The branches end their block and name their successors; the return ends the
\ routine and names none; the compare-and-branch defines no value at all.
: BRANCH-BODY ( IR-CTX:ctx -- bool n n bool n n bool n bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b X64IR-OPCODE:BR X64IR:OPCODE {: j:IR-ID:ir-symbol-id :}
   c b X64IR-OPCODE:CMPBR X64IR:OPCODE {: k:IR-ID:ir-symbol-id :}
   c b X64IR-OPCODE:BRZ X64IR:OPCODE {: z:IR-ID:ir-symbol-id :}
   c b X64IR-OPCODE:RET X64IR:OPCODE {: r:IR-ID:ir-symbol-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   rv j IR-SCHEMA:FTERMINATOR?
   rv j IR-SCHEMA:FSUCCESSORS
   rv k IR-SCHEMA:FSUCCESSORS
   rv k IR-SCHEMA:FTERMINATOR?
   rv k IR-SCHEMA:FRESULTS
   rv z IR-SCHEMA:FSUCCESSORS
   rv r IR-SCHEMA:FTERMINATOR?
   rv r IR-SCHEMA:FSUCCESSORS
   rv r IR-SCHEMA:FOPERAND-TAIL? ;

: BRANCH-CASE ( -- )
   s" the branches end their block and the return names no successor" T-LABEL
   BND [: BRANCH-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 0 T= TTRUE 2 T= 0 T= TTRUE 2 T= 1 T= TTRUE ;

\ The frame and data-stack forms carry a memory effect rather than purity, which
\ is what makes the freeze verifier demand the token they thread.
: MEMORY-BODY ( IR-CTX:ctx -- bool bool bool bool n n )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b X64IR-OPCODE:STORE X64IR:OPCODE {: w:IR-ID:ir-symbol-id :}
   c b X64IR-OPCODE:LOAD X64IR:OPCODE {: d:IR-ID:ir-symbol-id :}
   c b X64IR-OPCODE:DSTORE X64IR:OPCODE {: s:IR-ID:ir-symbol-id :}
   c b X64IR-OPCODE:CALL X64IR:OPCODE {: k:IR-ID:ir-symbol-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   rv w IR-SCHEMA:FEFFECT@ IR--SCHEMA-EFFECT:WRITE IR--SCHEMA-EFFECT:EQ
   rv d IR-SCHEMA:FEFFECT@ IR--SCHEMA-EFFECT:READ IR--SCHEMA-EFFECT:EQ
   rv s IR-SCHEMA:FEFFECT@ IR--SCHEMA-EFFECT:WRITE IR--SCHEMA-EFFECT:EQ
   rv k IR-SCHEMA:FEFFECT@ IR--SCHEMA-EFFECT:READ-WRITE IR--SCHEMA-EFFECT:EQ
   rv d IR-SCHEMA:FRESULTS
   rv k IR-SCHEMA:FATTRS ;

: MEMORY-CASE ( -- )
   s" the frame, data-stack and call forms declare the memory they touch" T-LABEL
   BND [: MEMORY-BODY ;] IR-CTX:WITH-CONTEXT
   2 T= 2 T= TTRUE TTRUE TTRUE TTRUE ;

\ ---- the operand refusals ----------------------------------------------------
\ A caller reaches every checked field through an attribute builder, so each
\ refusal is proved on the production word rather than on the bound alone.
: IMM-HIGH-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MOD-NEW {: b:IR-BUILD:builder :}
   c b X64IR:IMM-LIMIT X64IR:IMM32-ATTR drop ;

: IMM-LOW-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MOD-NEW {: b:IR-BUILD:builder :}
   c b X64IR:IMM-LIMIT negate 1- X64IR:IMM32-ATTR drop ;

: SHIFT-WIDE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MOD-NEW {: b:IR-BUILD:builder :}
   c b X64IR:SHIFT-LIMIT X64IR:SHIFT-ATTR drop ;

: SLOT-ODD-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MOD-NEW {: b:IR-BUILD:builder :}
   c b 4 X64IR:SLOT-ATTR drop ;

: SLOT-BACK-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MOD-NEW {: b:IR-BUILD:builder :}
   c b -8 X64IR:SLOT-ATTR drop ;

: FRAME-UNALIGNED-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MOD-NEW {: b:IR-BUILD:builder :}
   c b 8 X64IR:FRAME-ATTR drop ;

: DSLOT-ODD-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MOD-NEW {: b:IR-BUILD:builder :}
   c b -4 X64IR:DSLOT-ATTR drop ;

: DBYTES-ODD-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MOD-NEW {: b:IR-BUILD:builder :}
   c b 12 X64IR:DBYTES-ATTR drop ;

: ENTRY-ZERO-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MOD-NEW {: b:IR-BUILD:builder :}
   c b 0 X64IR:ENTRY-ATTR drop ;

: FUN-NEGATIVE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MOD-NEW {: b:IR-BUILD:builder :}
   c b -1 X64IR:FUN-ATTR drop ;

: IMM-HIGH ( -- )
   BND [: IMM-HIGH-BODY ;] IR-CTX:WITH-CONTEXT ;

: IMM-LOW ( -- )
   BND [: IMM-LOW-BODY ;] IR-CTX:WITH-CONTEXT ;

: SHIFT-WIDE ( -- )
   BND [: SHIFT-WIDE-BODY ;] IR-CTX:WITH-CONTEXT ;

: SLOT-ODD ( -- )
   BND [: SLOT-ODD-BODY ;] IR-CTX:WITH-CONTEXT ;

: SLOT-BACK ( -- )
   BND [: SLOT-BACK-BODY ;] IR-CTX:WITH-CONTEXT ;

: FRAME-UNALIGNED ( -- )
   BND [: FRAME-UNALIGNED-BODY ;] IR-CTX:WITH-CONTEXT ;

: DSLOT-ODD ( -- )
   BND [: DSLOT-ODD-BODY ;] IR-CTX:WITH-CONTEXT ;

: DBYTES-ODD ( -- )
   BND [: DBYTES-ODD-BODY ;] IR-CTX:WITH-CONTEXT ;

: ENTRY-ZERO ( -- )
   BND [: ENTRY-ZERO-BODY ;] IR-CTX:WITH-CONTEXT ;

: FUN-NEGATIVE ( -- )
   BND [: FUN-NEGATIVE-BODY ;] IR-CTX:WITH-CONTEXT ;

: FOREIGN-BODY ( IR-CTX:ctx -- )
   MOD-NEW drop ;

: FOREIGN ( -- )
   PBND [: FOREIGN-BODY ;] IR-CTX:WITH-CONTEXT ;

: REFUSE-CASE ( -- )
   s" an immediate outside the signed thirty-two bits is refused" T-LABEL
   [: IMM-HIGH ;] E-X64IR-IMM TTHROWSQ
   [: IMM-LOW ;] E-X64IR-IMM TTHROWSQ
   s" a shift count the machine would mask away is refused" T-LABEL
   [: SHIFT-WIDE ;] E-X64IR-SHIFT TTHROWSQ
   s" a frame slot off the cell or behind the frame is refused" T-LABEL
   [: SLOT-ODD ;] E-X64IR-SLOT TTHROWSQ
   [: SLOT-BACK ;] E-X64IR-SLOT TTHROWSQ
   s" a frame that breaks the stack alignment is refused" T-LABEL
   [: FRAME-UNALIGNED ;] E-X64IR-FRAME TTHROWSQ
   s" a data-stack offset or adjustment off the cell is refused" T-LABEL
   [: DSLOT-ODD ;] E-X64IR-DSLOT TTHROWSQ
   [: DBYTES-ODD ;] E-X64IR-DBYTES TTHROWSQ
   s" an entry address no routine has, and a function ordinal none has" T-LABEL
   [: ENTRY-ZERO ;] E-X64IR-ENTRY TTHROWSQ
   [: FUN-NEGATIVE ;] E-X64IR-FUN TTHROWSQ ;

\ ---- what this dialect admits that the ARM64 one does not --------------------
\ A cell already pushed is BEHIND the data-stack pointer, and an x86-64
\ displacement is signed, so a negative data-stack offset is an ordinary operand
\ here where ARM64 bounds it by its own reach.
: ADMIT-BODY ( IR-CTX:ctx -- n n n n )
   {: c:IR-CTX:ctx :}
   c MOD-NEW {: b:IR-BUILD:builder :}
   c b -8 X64IR:DSLOT-ATTR drop
   c b -16 X64IR:DBYTES-ATTR drop
   c b 16 X64IR:FRAME-ATTR drop
   c b 0 X64IR:SLOT-ATTR drop
   X64IR:IMM-LIMIT
   X64IR:SHIFT-LIMIT
   X64IR:DISP-LIMIT
   X64IR:SP-ALIGN ;

: ADMIT-CASE ( -- )
   s" a negative data-stack offset and a zero frame slot are ordinary" T-LABEL
   BND [: ADMIT-BODY ;] IR-CTX:WITH-CONTEXT
   16 T= X64IR:IMM-LIMIT 1- T= 64 T= 1 31 lshift T= ;

\ ---- the machine this dialect cannot build for -------------------------------
\ The registry refuses first and by its own name: PTX has no backend loaded in
\ this image, so CTARGET:LOWERS? throws E-CTGT-UNLOADED before this dialect is
\ asked whether it serves the machine. A loaded backend that declines a coherent
\ contract is the other refusal, E-IR-SCHEMA-TARGET, and no such x86-64 contract
\ exists to build here: src/compiler/target.f admits no big-endian or 32-bit
\ x86-64.
: TARGET-CASE ( -- )
   s" a machine with no backend loaded cannot own a module of this dialect" T-LABEL
   [: FOREIGN ;] E-CTGT-UNLOADED TTHROWSQ ;

\ A refusing case runs INSIDE an enclosing context: an abandoned context gives
\ its registry slots back only when a live enclosing context leaves normally.
: GROUP-REFUSE ( IR-CTX:ctx -- )
   drop
   REFUSE-CASE ;

: GROUP-TARGET ( IR-CTX:ctx -- )
   drop
   TARGET-CASE ;

\ A minimal machine with a distinct frame bound per row. Fill after all other
\ cases: a full table must still intern an existing description without writes.
: CAPACITY-MACHINE ( n -- NMACH:mach ) {: frame:n :}
   2 1 NREGFILE:REGS-SET NREGFILE:REGS-NONE
   1 NREGFILE:REGS-NONE NREGFILE:REGS-NONE 8 NREGFILE:FILE
   NREGFILE:REGS-NONE 0 16 frame 0 NMACH-OFFSCALE:BY-BYTE
   1 8 lshift 0 NMACH:MACHINE ;

: MACHINE-CAPACITY-CASE ( -- )
   s" a full machine table still returns an existing description" T-LABEL
   0 CAPACITY-MACHINE {: first:NMACH:mach :}
   16 first NMACH:ID - 1 ?do i 16 * CAPACITY-MACHINE drop loop
   0 CAPACITY-MACHINE first NMACH-MACH:EQ TTRUE
   s" a new description is refused without changing the full table" T-LABEL
   [: 4096 CAPACITY-MACHINE drop ;] E-NMACH TTHROWSQ
   0 CAPACITY-MACHINE first NMACH-MACH:EQ TTRUE
   [: 16 NMACH:BY-ID drop ;] E-NMACH TTHROWSQ ;

public

: RUN ( -- )
   T-RESET
   REGISTRY-CASE
   OPCODE-ORDINAL-CASE
   REGFILE-CASE
   MACHINE-CASE
   LOWERING-CASE
   COND-CASE
   MOVE-CASE
   TIE-CASE
   DIVIDE-CASE
   BRANCH-CASE
   MEMORY-CASE
   BND [: GROUP-REFUSE ;] IR-CTX:WITH-CONTEXT
   ADMIT-CASE
   BND [: GROUP-TARGET ;] IR-CTX:WITH-CONTEXT
   MACHINE-CAPACITY-CASE
   T-REPORT ;

;package

X64IR-TEST:RUN

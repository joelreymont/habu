\ native-a64ir.f - checked ARM64 machine dialect tests.
\
\ Proves the contract of src/compiler/native/a64ir.f: registering the dialect
\ defines exactly ten opcodes and every declared field of each one reads back
\ through the frozen schema table; the two move-wide operand bounds and the two
\ frame-slot bounds are the shipped assembler's own and are asserted against it
\ rather than restated; a move-wide immediate or shift, a frame slot, or a
\ reserved frame outside its field is refused before it can be interned as an
\ attribute; the halves of a 64-bit value are read out the way a move-wide chain
\ has to reproduce them; and a module of another dialect, a second registration,
\ and a target this dialect cannot run on are each refused by name.
\
\ WHY THE BOUNDS ARE ASSERTED AGAINST THE ASSEMBLER. The dialect writes its
\ bounds as the field widths they are, because requiring the whole shipped
\ assembler into a compiler module would be the wrong dependency. This suite
\ requires both and asserts each dialect bound against the encoder constant it
\ was derived from, so a field that changed width in src/arch/arm64/asm.f
\ reddens here instead of letting the two disagree quietly.
\
\ WHY THE MAY-TRAP FLAG IS ASSERTED UNDER A TRAPPING POLICY. The HIR dialect's
\ arithmetic follows the compilation unit's overflow policy, because whether a
\ Habu `+` traps is the unit's decision. A machine Add does not: ARM64 wraps.
\ The trapping binding below is therefore the one the schema is read under, so
\ "no form of this dialect traps" is measured where it could most easily have
\ been copied from the unit instead.

require lib/test.f
require src/compiler/a64-effect.f
require src/compiler/native/a64ir.f
require src/arch/arm64/asm.f

package A64IR-TEST
private

\ ---- bindings ----------------------------------------------------------------
\ An AArch64 Darwin contract whose integer overflow traps.
: BND ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ A GPU kernel contract: this dialect is the native pipeline's, so it must not
\ register here at all.
: PBND ( -- CBIND:binding )
   CTARGET-ARCH:PTX CTARGET-ABI:PTX-KERNEL CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ ---- module rigging ----------------------------------------------------------
: MOD-NEW ( IR-CTX:ctx -- IR-BUILD:builder )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   c A64IR:NEW-BUILDER ;

: DIALECT-NEW ( IR-CTX:ctx -- IR-BUILD:builder )
   {: c:IR-CTX:ctx :}
   c MOD-NEW {: b:IR-BUILD:builder :}
   c b A64IR:REGISTER
   b ;

\ ---- the machine bounds ------------------------------------------------------
\ IMM16-LIM and HW-LIM are the shipped encoder's own field bounds; the dialect
\ derived its two from the same widths and must still agree with them.
: BOUND-CASE ( -- )
   s" the move-wide bounds are the shipped assembler's" T-LABEL
   A64IR:IMM-LIMIT IMM16-LIM T=
   A64IR:HALVES HW-LIM T=
   A64IR:HALVES A64IR:HALF-BITS * A64IR:REG-BITS T=
   A64IR:HALF-BITS 16 T= ;

\ ---- the halves of a value ---------------------------------------------------
\ A negative value has to read as the bit pattern the machine holds, because a
\ move-wide chain reproduces bits and not arithmetic.
: HALVES-CASE ( -- )
   s" a value reads out as the four halves a move-wide chain writes" T-LABEL
   $123456789ABCDEF0 0 A64IR:HALF-OF $DEF0 T=
   $123456789ABCDEF0 1 A64IR:HALF-OF $9ABC T=
   $123456789ABCDEF0 2 A64IR:HALF-OF $5678 T=
   $123456789ABCDEF0 3 A64IR:HALF-OF $1234 T=
   -1 3 A64IR:HALF-OF $FFFF T=
   0 0 A64IR:HALF-OF 0 T=
   0 A64IR:HALF-SHIFT 0 T=
   1 A64IR:HALF-SHIFT 16 T=
   2 A64IR:HALF-SHIFT 32 T=
   3 A64IR:HALF-SHIFT 48 T= ;

\ ---- the frame bounds --------------------------------------------------------
\ A frame slot is reached by an unsigned-offset store whose byte offset the
\ encoder divides by the access width, so two facts have to agree with the
\ shipped assembler and not merely be written down here: the width the dialect
\ places slots at is the width the encoder scales by, and the deepest slot the
\ dialect will accept is the last one the twelve-bit field holds. Both are read
\ off ENC-STR's own output rather than restated, so an encoder that changed its
\ scale or its field reddens here.
: STR-OFFSET-FIELD ( n -- n )
   {: off:n :}
   0 A64EFF:ZERO-GPR off ENC-STR 10 rshift $FFF and ;

: FRAME-BOUND-CASE ( -- )
   s" the frame-slot bounds are the shipped assembler's" T-LABEL
   A64IR:SLOT-WIDTH STR-OFFSET-FIELD 1 T=
   A64IR:SLOT-WIDTH A64EFF:SLOT-REACH STR-OFFSET-FIELD IMM12-LIM 1- T=
   A64IR:SLOT-WIDTH A64EFF:SLOT-REACH  IMM12-LIM 1- A64IR:SLOT-WIDTH * T=
   A64IR:SLOT-WIDTH 8 T= ;

\ ---- registration ------------------------------------------------------------
\ The ten opcodes, and the count, so "nothing else was defined" is measured
\ rather than assumed.
: COUNT-BODY ( IR-CTX:ctx -- n bool bool bool bool bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b A64IR-OPCODE:MOVZ A64IR:OPCODE {: z:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:MOVK A64IR:OPCODE {: k:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:ADD A64IR:OPCODE {: a:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:SUB A64IR:OPCODE {: s:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:MUL A64IR:OPCODE {: u:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:STORE A64IR:OPCODE {: w:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:LOAD A64IR:OPCODE {: d:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:RESERVE A64IR:OPCODE {: p:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:RELEASE A64IR:OPCODE {: q:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:RET A64IR:OPCODE {: t:IR-ID:ir-symbol-id :}
   b IR-BUILD:SCHEMAS
   c b IR-BUILD:FREEZE IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   rv z IR-SCHEMA:FDEFINED?
   rv k IR-SCHEMA:FDEFINED?
   rv a IR-SCHEMA:FDEFINED?
   rv s IR-SCHEMA:FDEFINED?
   rv u IR-SCHEMA:FDEFINED?
   rv w IR-SCHEMA:FDEFINED?
   rv d IR-SCHEMA:FDEFINED?
   rv p IR-SCHEMA:FDEFINED?
   rv q IR-SCHEMA:FDEFINED?
   rv t IR-SCHEMA:FDEFINED? ;

: COUNT-CASE ( -- )
   s" registration defines exactly the ten machine opcodes" T-LABEL
   BND [: COUNT-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE 10 T= ;

\ The dialect names its own table: a caller never spells the name or the version.
: NAMED-BODY ( IR-CTX:ctx -- bool n n )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   b IR-BUILD:MODULE-KEY {: key:IR-ID:ir-module-key :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FSYM-POOL {: pv:IR-ARENA:view :}
   m IR-BUILD:FSYM-ROWS {: yv:IR-ARENA:view :}
   m IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   pv yv  rv key IR-SCHEMA:FDIALECT@  s" a64" IR-SYM:FEQ?
   rv IR-SCHEMA:FMAJOR@
   rv IR-SCHEMA:FMINOR@ ;

: NAMED-CASE ( -- )
   s" the schema table carries the dialect's own name and version" T-LABEL
   BND [: NAMED-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 0 T= TTRUE ;

\ The spellings themselves, because every reference this dialect stores is a
\ symbol and a renamed opcode would still read back through the same accessor.
: SPELL-BODY ( IR-CTX:ctx -- bool bool bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b A64IR-OPCODE:MOVZ A64IR:OPCODE {: z:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:MOVK A64IR:OPCODE {: k:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:ADD A64IR:OPCODE {: a:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:SUB A64IR:OPCODE {: s:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:MUL A64IR:OPCODE {: u:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:RET A64IR:OPCODE {: t:IR-ID:ir-symbol-id :}
   c b A64IR:KEY-IMM {: ik:IR-ID:ir-symbol-id :}
   c b A64IR:KEY-SHIFT {: sk:IR-ID:ir-symbol-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FSYM-POOL {: pv:IR-ARENA:view :}
   m IR-BUILD:FSYM-ROWS {: yv:IR-ARENA:view :}
   pv yv z s" a64.movz" IR-SYM:FEQ?
   pv yv k s" a64.movk" IR-SYM:FEQ?
   pv yv a s" a64.add" IR-SYM:FEQ?
   pv yv s s" a64.sub" IR-SYM:FEQ?
   pv yv u s" a64.mul" IR-SYM:FEQ?
   pv yv t s" a64.ret" IR-SYM:FEQ?
   pv yv ik s" a64.imm" IR-SYM:FEQ?
   pv yv sk s" a64.shift" IR-SYM:FEQ? ;

: SPELL-CASE ( -- )
   s" the arithmetic opcodes and the two move-wide keys are spelled as declared" T-LABEL
   BND [: SPELL-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE ;

\ The four frame forms and their two keys, spelled the way the instruction
\ vocabulary spells them. The two the enum cannot spell - `str` and `ldr` are
\ taken names in this Forth - are named `store` and `load` in the family and keep
\ the assembler's mnemonic as their symbol, which is what every other reader
\ sees.
: FRAME-SPELL-BODY ( IR-CTX:ctx -- bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b A64IR-OPCODE:STORE A64IR:OPCODE {: w:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:LOAD A64IR:OPCODE {: d:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:RESERVE A64IR:OPCODE {: p:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:RELEASE A64IR:OPCODE {: q:IR-ID:ir-symbol-id :}
   c b A64IR:KEY-SLOT {: lk:IR-ID:ir-symbol-id :}
   c b A64IR:KEY-FRAME {: fk:IR-ID:ir-symbol-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FSYM-POOL {: pv:IR-ARENA:view :}
   m IR-BUILD:FSYM-ROWS {: yv:IR-ARENA:view :}
   pv yv w s" a64.str" IR-SYM:FEQ?
   pv yv d s" a64.ldr" IR-SYM:FEQ?
   pv yv p s" a64.reserve" IR-SYM:FEQ?
   pv yv q s" a64.release" IR-SYM:FEQ?
   pv yv lk s" a64.slot" IR-SYM:FEQ?
   pv yv fk s" a64.frame" IR-SYM:FEQ? ;

: FRAME-SPELL-CASE ( -- )
   s" the four frame opcodes and their two keys are spelled as declared" T-LABEL
   BND [: FRAME-SPELL-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE ;

\ ---- the declared shapes -----------------------------------------------------
\ Every field the arithmetic schema declares, read back off the frozen table.
\ The may-trap flag is false under the trapping binding above: a machine Add
\ wraps whatever the compilation unit asked of a Habu `+`.
: ARITH-BODY ( IR-CTX:ctx -- n bool n bool n n n bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   b IR-BUILD:MODULE-KEY {: key:IR-ID:ir-module-key :}
   c b A64IR-OPCODE:SUB A64IR:OPCODE {: op:IR-ID:ir-symbol-id :}
   c b A64IR:GPR-TYPE {: t:IR-ID:ir-type-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FSCHEMA-POOL {: qv:IR-ARENA:view :}
   m IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   rv op IR-SCHEMA:FOPERANDS
   rv op IR-SCHEMA:FOPERAND-TAIL?
   rv op IR-SCHEMA:FRESULTS
   rv op IR-SCHEMA:FRESULT-TAIL?
   rv op IR-SCHEMA:FSUCCESSORS
   rv op IR-SCHEMA:FATTRS
   rv op IR-SCHEMA:FREGIONS
   rv op IR-SCHEMA:FTERMINATOR?
   rv op IR-SCHEMA:FTRAPS?
   rv op IR-SCHEMA:FEFFECT@ IR--SCHEMA-EFFECT:PURE IR--SCHEMA-EFFECT:EQ
   rv op IR-SCHEMA:FARCH@ CTARGET-ARCH:AARCH64 CTARGET-ARCH:EQ
   qv rv key op 1 IR-SCHEMA:FOPERAND@ IR-ID:TYPE-LOCAL t IR-ID:TYPE-LOCAL =
   qv rv key op 0 IR-SCHEMA:FRESULT@ IR-ID:TYPE-LOCAL t IR-ID:TYPE-LOCAL = ;

: ARITH-CASE ( -- )
   s" a shifted-register opcode reads back exactly as declared" T-LABEL
   BND [: ARITH-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE
   TFALSE TFALSE 0 T= 0 T= 0 T= TFALSE 1 T= TFALSE 2 T= ;

\ The two moves and the return: movz writes a register nothing read, movk keeps
\ the value it was handed, and the return is a terminator that takes the live
\ values and has no results of its own.
: SHAPE-BODY ( IR-CTX:ctx -- n n n bool bool n n n bool bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   b IR-BUILD:MODULE-KEY {: key:IR-ID:ir-module-key :}
   c b A64IR-OPCODE:MOVZ A64IR:OPCODE {: z:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:MOVK A64IR:OPCODE {: k:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:RET A64IR:OPCODE {: r:IR-ID:ir-symbol-id :}
   c b A64IR:KEY-IMM {: ik:IR-ID:ir-symbol-id :}
   c b A64IR:KEY-SHIFT {: sk:IR-ID:ir-symbol-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FSCHEMA-POOL {: qv:IR-ARENA:view :}
   m IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   rv z IR-SCHEMA:FOPERANDS
   rv z IR-SCHEMA:FRESULTS
   rv z IR-SCHEMA:FATTRS
   qv rv key z 0 IR-SCHEMA:FATTR@ IR-ID:SYMBOL-LOCAL ik IR-ID:SYMBOL-LOCAL =
   qv rv key z 1 IR-SCHEMA:FATTR@ IR-ID:SYMBOL-LOCAL sk IR-ID:SYMBOL-LOCAL =
   rv k IR-SCHEMA:FOPERANDS
   rv r IR-SCHEMA:FOPERANDS
   rv r IR-SCHEMA:FRESULTS
   rv r IR-SCHEMA:FOPERAND-TAIL?
   rv r IR-SCHEMA:FTERMINATOR? ;

: SHAPE-CASE ( -- )
   s" the moves and the return have the shapes their forms have" T-LABEL
   BND [: SHAPE-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE 0 T= 1 T= 1 T= TTRUE TTRUE 2 T= 1 T= 0 T= ;

\ ---- the declared shapes of the frame forms ----------------------------------
\ The store puts a register away and passes the memory order on; the load takes
\ the order, answers the register first and the order second; the reserve mints
\ the order out of nothing and the release ends it. Every one of them declares a
\ memory effect rather than purity, which is what makes the freeze verifier
\ demand the token they carry - so this case is also what proves the token is not
\ decoration.
: FRAME-SHAPE-BODY ( IR-CTX:ctx -- n n n bool bool bool bool n n bool bool bool n n n n bool bool bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   b IR-BUILD:MODULE-KEY {: key:IR-ID:ir-module-key :}
   c b A64IR-OPCODE:STORE A64IR:OPCODE {: w:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:LOAD A64IR:OPCODE {: d:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:RESERVE A64IR:OPCODE {: p:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:RELEASE A64IR:OPCODE {: q:IR-ID:ir-symbol-id :}
   c b A64IR:GPR-TYPE {: t:IR-ID:ir-type-id :}
   c b A64IR:MEM-TYPE {: kt:IR-ID:ir-type-id :}
   c b A64IR:KEY-SLOT {: lk:IR-ID:ir-symbol-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FSCHEMA-POOL {: qv:IR-ARENA:view :}
   m IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   rv w IR-SCHEMA:FOPERANDS
   rv w IR-SCHEMA:FRESULTS
   rv w IR-SCHEMA:FATTRS
   qv rv key w 0 IR-SCHEMA:FATTR@ IR-ID:SYMBOL-LOCAL lk IR-ID:SYMBOL-LOCAL =
   rv w IR-SCHEMA:FEFFECT@ IR--SCHEMA-EFFECT:WRITE IR--SCHEMA-EFFECT:EQ
   qv rv key w 0 IR-SCHEMA:FOPERAND@ IR-ID:TYPE-LOCAL t IR-ID:TYPE-LOCAL =
   qv rv key w 1 IR-SCHEMA:FOPERAND@ IR-ID:TYPE-LOCAL kt IR-ID:TYPE-LOCAL =
   rv d IR-SCHEMA:FOPERANDS
   rv d IR-SCHEMA:FRESULTS
   qv rv key d 0 IR-SCHEMA:FRESULT@ IR-ID:TYPE-LOCAL t IR-ID:TYPE-LOCAL =
   qv rv key d 1 IR-SCHEMA:FRESULT@ IR-ID:TYPE-LOCAL kt IR-ID:TYPE-LOCAL =
   rv d IR-SCHEMA:FEFFECT@ IR--SCHEMA-EFFECT:READ IR--SCHEMA-EFFECT:EQ
   rv p IR-SCHEMA:FOPERANDS
   rv p IR-SCHEMA:FRESULTS
   rv q IR-SCHEMA:FOPERANDS
   rv q IR-SCHEMA:FRESULTS
   rv w IR-SCHEMA:FTERMINATOR?
   rv w IR-SCHEMA:FTRAPS?
   rv w IR-SCHEMA:FALIAS@ IR--SCHEMA-ALIAS:UNALIASED IR--SCHEMA-ALIAS:EQ ;

: FRAME-SHAPE-CASE ( -- )
   s" the four frame forms have the shapes their instructions have" T-LABEL
   BND [: FRAME-SHAPE-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TFALSE TFALSE 0 T= 1 T= 1 T= 0 T=
   TTRUE TTRUE TTRUE 2 T= 1 T=
   TTRUE TTRUE TTRUE TTRUE 1 T= 1 T= 2 T= ;

\ ---- the tied register field -------------------------------------------------
\ The move-wide overwrite is the one form of this dialect whose result and whose
\ operand are one register field, and its schema is where that is written down.
\ Every other form names each of its registers once, so a consumer that reads the
\ tie gets the constraint from the form instead of from an opcode's name.
: TIE-BODY ( IR-CTX:ctx -- n n n n n n n n )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b A64IR-OPCODE:MOVZ A64IR:OPCODE {: z:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:MOVK A64IR:OPCODE {: k:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:ADD A64IR:OPCODE {: a:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:SUB A64IR:OPCODE {: s:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:MUL A64IR:OPCODE {: u:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:RET A64IR:OPCODE {: t:IR-ID:ir-symbol-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FSCHEMA-POOL {: qv:IR-ARENA:view :}
   m IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   rv k IR-SCHEMA:FTIES
   qv rv k 0 IR-SCHEMA:FTIE-RESULT@
   qv rv k 0 IR-SCHEMA:FTIE-OPERAND@
   rv z IR-SCHEMA:FTIES
   rv a IR-SCHEMA:FTIES
   rv s IR-SCHEMA:FTIES
   rv u IR-SCHEMA:FTIES
   rv t IR-SCHEMA:FTIES ;

: TIE-CASE ( -- )
   s" the move-wide overwrite declares its one tie and nothing else does" T-LABEL
   BND [: TIE-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 0 T= 0 T= 0 T= 0 T= 0 T= 0 T= 1 T= ;

\ ---- the move-wide operand refusals ------------------------------------------
\ A caller reaches the two operand fields only through the attribute builders, so
\ the refusal is proved on the production word rather than on the bound alone.
: IMM-HIGH-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b A64IR:IMM-LIMIT A64IR:IMM-ATTR drop ;

: IMM-LOW-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b -1 A64IR:IMM-ATTR drop ;

: SHIFT-ODD-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b 8 A64IR:SHIFT-ATTR drop ;

: SHIFT-HIGH-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b A64IR:REG-BITS A64IR:SHIFT-ATTR drop ;

\ ---- the frame operand refusals ----------------------------------------------
\ A slot the memory forms cannot address and a frame no routine can declare are
\ both refused on the production builder, so no module can hold one at all.
: SLOT-ODD-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b A64IR:SLOT-WIDTH 1- A64IR:SLOT-ATTR drop ;

: SLOT-LOW-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b A64IR:SLOT-WIDTH negate A64IR:SLOT-ATTR drop ;

: SLOT-HIGH-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b  A64IR:SLOT-WIDTH A64EFF:SLOT-REACH A64IR:SLOT-WIDTH +  A64IR:SLOT-ATTR
   drop ;

: FRAME-ODD-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b A64IR:SLOT-WIDTH A64IR:FRAME-ATTR drop ;

: FRAME-HIGH-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b  A64EFF:FRAME-MAX A64EFF:SP-ALIGN +  A64IR:FRAME-ATTR drop ;

\ A frame inside the region A64EFF can describe and past the one immediate that
\ claims it. The two bounds are different fields - a slot offset is scaled by the
\ access width and the frame immediate is not - so a frame between them is the
\ only case that reaches the second bound at all.
: FRAME-DEEP-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b  A64IR:FRAME-LIMIT A64EFF:SP-ALIGN +  A64IR:FRAME-ATTR drop ;

: SLOT-ODD ( -- )
   BND [: SLOT-ODD-BODY ;] IR-CTX:WITH-CONTEXT ;

: SLOT-LOW ( -- )
   BND [: SLOT-LOW-BODY ;] IR-CTX:WITH-CONTEXT ;

: SLOT-HIGH ( -- )
   BND [: SLOT-HIGH-BODY ;] IR-CTX:WITH-CONTEXT ;

: FRAME-ODD ( -- )
   BND [: FRAME-ODD-BODY ;] IR-CTX:WITH-CONTEXT ;

: FRAME-HIGH ( -- )
   BND [: FRAME-HIGH-BODY ;] IR-CTX:WITH-CONTEXT ;

: FRAME-DEEP ( -- )
   BND [: FRAME-DEEP-BODY ;] IR-CTX:WITH-CONTEXT ;

: SLOT-REFUSE-CASES ( -- )
   s" a frame slot that is not a whole access from the frame is refused" T-LABEL
   [: SLOT-ODD ;] E-A64IR-SLOT TTHROWSQ
   s" a negative frame slot is refused" T-LABEL
   [: SLOT-LOW ;] E-A64IR-SLOT TTHROWSQ ;

: FRAME-REFUSE-CASES ( -- )
   s" a frame slot past the reach of the offset field is refused" T-LABEL
   [: SLOT-HIGH ;] E-A64IR-SLOT TTHROWSQ
   s" a frame that does not keep the stack pointer aligned is refused" T-LABEL
   [: FRAME-ODD ;] E-A64IR-FRAME TTHROWSQ ;

\ The two frame depths are a group of their own: each of these abandons a
\ context holding a module, and the registry gives those slots back only when a
\ live enclosing context leaves.
: FRAME-DEPTH-CASES ( -- )
   s" a frame deeper than the offset field can reach is refused" T-LABEL
   [: FRAME-HIGH ;] E-A64IR-FRAME TTHROWSQ
   s" a frame deeper than the immediate that claims it is refused" T-LABEL
   [: FRAME-DEEP ;] E-A64IR-FRAME TTHROWSQ ;

: IMM-HIGH ( -- )
   BND [: IMM-HIGH-BODY ;] IR-CTX:WITH-CONTEXT ;

: IMM-LOW ( -- )
   BND [: IMM-LOW-BODY ;] IR-CTX:WITH-CONTEXT ;

: SHIFT-ODD ( -- )
   BND [: SHIFT-ODD-BODY ;] IR-CTX:WITH-CONTEXT ;

: SHIFT-HIGH ( -- )
   BND [: SHIFT-HIGH-BODY ;] IR-CTX:WITH-CONTEXT ;

: HALF-HIGH ( -- )
   0 A64IR:HALVES A64IR:HALF-OF drop ;

: SHIFT-INDEX-HIGH ( -- )
   A64IR:HALVES A64IR:HALF-SHIFT drop ;

: IMM-REFUSE-CASES ( -- )
   s" a move-wide immediate past the sixteen-bit field is refused" T-LABEL
   [: IMM-HIGH ;] E-A64IR-IMM TTHROWSQ
   s" a negative move-wide immediate is refused" T-LABEL
   [: IMM-LOW ;] E-A64IR-IMM TTHROWSQ ;

: SHIFT-REFUSE-CASES ( -- )
   s" a shift that does not select a half is refused" T-LABEL
   [: SHIFT-ODD ;] E-A64IR-SHIFT TTHROWSQ
   s" a shift past the register width is refused" T-LABEL
   [: SHIFT-HIGH ;] E-A64IR-SHIFT TTHROWSQ
   s" a half index past the four halves is refused" T-LABEL
   [: HALF-HIGH ;] E-A64IR-SHIFT TTHROWSQ
   [: SHIFT-INDEX-HIGH ;] E-A64IR-SHIFT TTHROWSQ ;

\ ---- registration refusals ---------------------------------------------------
\ A module created for another dialect holds another dialect's closed world, and
\ its schema table must not gain these rows even though nothing else stops it.
: FOREIGN-DIALECT-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   c s" hir" 0 1 IR-BUILD:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b A64IR:REGISTER ;

: FOREIGN-VERSION-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN
   IR-BUILD:PLAN-DEFAULT
   c A64IR:NAME 9 9 IR-BUILD:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b A64IR:REGISTER ;

: TWICE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b A64IR:REGISTER ;

: PTX-BODY ( IR-CTX:ctx -- )
   DIALECT-NEW drop ;

: FOREIGN-DIALECT ( -- )
   BND [: FOREIGN-DIALECT-BODY ;] IR-CTX:WITH-CONTEXT ;

: FOREIGN-VERSION ( -- )
   BND [: FOREIGN-VERSION-BODY ;] IR-CTX:WITH-CONTEXT ;

: TWICE ( -- )
   BND [: TWICE-BODY ;] IR-CTX:WITH-CONTEXT ;

: PTX-REG ( -- )
   PBND [: PTX-BODY ;] IR-CTX:WITH-CONTEXT ;

: TABLE-REFUSE-CASES ( -- )
   s" a module of another dialect cannot hold the machine opcodes" T-LABEL
   [: FOREIGN-DIALECT ;] E-A64IR-DIALECT TTHROWSQ
   s" a module at another schema version cannot hold them either" T-LABEL
   [: FOREIGN-VERSION ;] E-A64IR-DIALECT TTHROWSQ ;

: TARGET-REFUSE-CASES ( -- )
   s" registering the dialect twice into one module is refused" T-LABEL
   [: TWICE ;] E-IR-SCHEMA-DUP TTHROWSQ
   s" the machine dialect refuses to register against a GPU target" T-LABEL
   [: PTX-REG ;] E-IR-SCHEMA-TARGET TTHROWSQ ;

\ ---- groups ------------------------------------------------------------------
\ A refused fixture leaves its context abandoned, and an abandoned context gives
\ its registry slots back only when an enclosing live context leaves normally
\ (src/compiler/ir/context.f, the note on stale handles). Every group therefore
\ runs inside one, refusal groups included, and a group holds few enough
\ abandoned modules to stay inside the arena registry while it is open.
: GROUP-REGISTER ( IR-CTX:ctx -- )
   drop
   COUNT-CASE
   NAMED-CASE
   SPELL-CASE ;

: GROUP-FRAME-SPELL ( IR-CTX:ctx -- )
   drop
   FRAME-SPELL-CASE ;

: GROUP-SHAPE ( IR-CTX:ctx -- )
   drop
   ARITH-CASE
   SHAPE-CASE ;

: GROUP-FRAME-SHAPE ( IR-CTX:ctx -- )
   drop
   FRAME-SHAPE-CASE ;

: GROUP-SLOT-REFUSE ( IR-CTX:ctx -- )
   drop
   SLOT-REFUSE-CASES ;

: GROUP-FRAME-REFUSE ( IR-CTX:ctx -- )
   drop
   FRAME-REFUSE-CASES ;

: GROUP-FRAME-DEPTH ( IR-CTX:ctx -- )
   drop
   FRAME-DEPTH-CASES ;

: GROUP-TIE ( IR-CTX:ctx -- )
   drop
   TIE-CASE ;

: GROUP-IMM-REFUSE ( IR-CTX:ctx -- )
   drop
   IMM-REFUSE-CASES ;

: GROUP-SHIFT-REFUSE ( IR-CTX:ctx -- )
   drop
   SHIFT-REFUSE-CASES ;

: GROUP-TABLE-REFUSE ( IR-CTX:ctx -- )
   drop
   TABLE-REFUSE-CASES ;

: GROUP-TARGET-REFUSE ( IR-CTX:ctx -- )
   drop
   TARGET-REFUSE-CASES ;

public

: RUN ( -- )
   T-RESET
   BOUND-CASE
   FRAME-BOUND-CASE
   s" the deepest frame this dialect can reserve is the add-sub immediate" T-LABEL
   A64IR:FRAME-LIMIT  IMM12-LIM 1- dup A64EFF:SP-ALIGN mod -  T=
   HALVES-CASE
   BND [: GROUP-REGISTER ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-FRAME-SPELL ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-SHAPE ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-FRAME-SHAPE ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-TIE ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-IMM-REFUSE ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-SLOT-REFUSE ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-FRAME-REFUSE ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-FRAME-DEPTH ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-SHIFT-REFUSE ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-TABLE-REFUSE ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-TARGET-REFUSE ;] IR-CTX:WITH-CONTEXT
   T-REPORT ;

;package

A64IR-TEST:RUN

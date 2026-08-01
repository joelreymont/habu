\ native-a64ir.f - checked ARM64 machine dialect tests.
\
\ Proves the contract of src/compiler/native/a64ir.f: registering the dialect
\ defines exactly eleven opcodes and every declared field of each one reads back
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
\ The opcodes, and the count, so "nothing else was defined" is measured rather
\ than assumed. The fourteen the register conventions use are here; the four that
\ reach the caller's data stack are checked in DSTACK-SPELL-CASE below and the
\ two addressed forms in ADDR-SHAPE-CASE, and the count covers all twenty.
: COUNT-BODY ( IR-CTX:ctx -- n bool bool bool bool bool bool bool bool bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b A64IR-OPCODE:MOVZ A64IR:OPCODE {: z:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:MOVK A64IR:OPCODE {: k:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:MOV A64IR:OPCODE {: v:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:ADD A64IR:OPCODE {: a:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:SUB A64IR:OPCODE {: s:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:MUL A64IR:OPCODE {: u:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:STORE A64IR:OPCODE {: w:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:LOAD A64IR:OPCODE {: d:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:RESERVE A64IR:OPCODE {: p:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:RELEASE A64IR:OPCODE {: q:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:FLAG A64IR:OPCODE {: g:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:BR A64IR:OPCODE {: j:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:BRZ A64IR:OPCODE {: n:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:RET A64IR:OPCODE {: t:IR-ID:ir-symbol-id :}
   b IR-BUILD:SCHEMAS
   c b IR-BUILD:FREEZE IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   rv z IR-SCHEMA:FDEFINED?
   rv k IR-SCHEMA:FDEFINED?
   rv v IR-SCHEMA:FDEFINED?
   rv a IR-SCHEMA:FDEFINED?
   rv s IR-SCHEMA:FDEFINED?
   rv u IR-SCHEMA:FDEFINED?
   rv w IR-SCHEMA:FDEFINED?
   rv d IR-SCHEMA:FDEFINED?
   rv p IR-SCHEMA:FDEFINED?
   rv q IR-SCHEMA:FDEFINED?
   rv g IR-SCHEMA:FDEFINED?
   rv j IR-SCHEMA:FDEFINED?
   rv n IR-SCHEMA:FDEFINED?
   rv t IR-SCHEMA:FDEFINED? ;

: COUNT-CASE ( -- )
   s" registration defines exactly the twenty machine opcodes" T-LABEL
   BND [: COUNT-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE
   TTRUE TTRUE TTRUE TTRUE 20 T= ;

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

\ The four data-stack forms and their two keys. They are their own opcodes and
\ their own keys, not the frame's: a routine reading an argument out of the
\ caller's stack and a routine reloading a spilled value are two different
\ accesses counted from two different pointers, and a reader has to be able to
\ tell them apart without asking which opcode it has.
: DSTACK-SPELL-BODY ( IR-CTX:ctx -- bool bool bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b A64IR-OPCODE:DTAKE A64IR:OPCODE {: tk:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:DLOAD A64IR:OPCODE {: ld:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:DSTORE A64IR:OPCODE {: st:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:DPUBLISH A64IR:OPCODE {: pb:IR-ID:ir-symbol-id :}
   c b A64IR:KEY-DSLOT {: sk:IR-ID:ir-symbol-id :}
   c b A64IR:KEY-DBYTES {: bk:IR-ID:ir-symbol-id :}
   b IR-BUILD:SCHEMAS drop
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   m IR-BUILD:FSYM-POOL {: pv:IR-ARENA:view :}
   m IR-BUILD:FSYM-ROWS {: yv:IR-ARENA:view :}
   rv tk IR-SCHEMA:FDEFINED?
   rv ld IR-SCHEMA:FDEFINED?
   rv st IR-SCHEMA:FDEFINED?
   rv pb IR-SCHEMA:FDEFINED?
   pv yv tk s" a64.dtake" IR-SYM:FEQ?
   pv yv ld s" a64.dload" IR-SYM:FEQ?
   pv yv sk s" a64.dslot" IR-SYM:FEQ?
   pv yv bk s" a64.dbytes" IR-SYM:FEQ? ;

: DSTACK-SPELL-CASE ( -- )
   s" the four data-stack opcodes and their two keys are spelled as declared" T-LABEL
   BND [: DSTACK-SPELL-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE ;

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

\ ---- the declared shapes of the two addressed forms --------------------------
\ These are the forms whose base is a value, so what has to be asserted is
\ exactly the two things a frame access does not have: an address OPERAND, and no
\ slot attribute at all. The load takes the address then the order and answers
\ the loaded register then the order; the store takes the value, the address and
\ the order and answers the order. Both are in the generic space with
\ unrestricted aliasing, which is the declaration that puts them on one chain
\ with the data-stack forms - and the data-stack store is read back here beside
\ them, because an unaliased data stack next to an unrestricted addressed store
\ would be the module claiming an independence it has no proof of.
: ADDR-SHAPE-BODY ( IR-CTX:ctx -- bool bool n n n bool bool bool n n n bool bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   b IR-BUILD:MODULE-KEY {: key:IR-ID:ir-module-key :}
   c b A64IR-OPCODE:ALOAD A64IR:OPCODE {: al:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:ASTORE A64IR:OPCODE {: as:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:DSTORE A64IR:OPCODE {: ds:IR-ID:ir-symbol-id :}
   c b A64IR:GPR-TYPE {: t:IR-ID:ir-type-id :}
   c b A64IR:MEM-TYPE {: kt:IR-ID:ir-type-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FSYM-POOL {: pv:IR-ARENA:view :}
   m IR-BUILD:FSYM-ROWS {: yv:IR-ARENA:view :}
   m IR-BUILD:FSCHEMA-POOL {: qv:IR-ARENA:view :}
   m IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   pv yv al s" a64.aldr" IR-SYM:FEQ?
   pv yv as s" a64.astr" IR-SYM:FEQ?
   rv al IR-SCHEMA:FOPERANDS
   rv al IR-SCHEMA:FRESULTS
   rv al IR-SCHEMA:FATTRS
   qv rv key al 0 IR-SCHEMA:FOPERAND@ IR-ID:TYPE-LOCAL t IR-ID:TYPE-LOCAL =
   qv rv key al 1 IR-SCHEMA:FOPERAND@ IR-ID:TYPE-LOCAL kt IR-ID:TYPE-LOCAL =
   rv al IR-SCHEMA:FEFFECT@ IR--SCHEMA-EFFECT:READ IR--SCHEMA-EFFECT:EQ
   rv as IR-SCHEMA:FOPERANDS
   rv as IR-SCHEMA:FRESULTS
   rv as IR-SCHEMA:FATTRS
   qv rv key as 0 IR-SCHEMA:FOPERAND@ IR-ID:TYPE-LOCAL t IR-ID:TYPE-LOCAL =
   qv rv key as 1 IR-SCHEMA:FOPERAND@ IR-ID:TYPE-LOCAL t IR-ID:TYPE-LOCAL =
   qv rv key as 2 IR-SCHEMA:FOPERAND@ IR-ID:TYPE-LOCAL kt IR-ID:TYPE-LOCAL =
   rv as IR-SCHEMA:FEFFECT@ IR--SCHEMA-EFFECT:WRITE IR--SCHEMA-EFFECT:EQ
   rv al IR-SCHEMA:FSPACE@ IR--TYPE-SPACE:GENERIC IR--TYPE-SPACE:EQ
   rv as IR-SCHEMA:FALIAS@ IR--SCHEMA-ALIAS:UNRESTRICTED IR--SCHEMA-ALIAS:EQ
   rv ds IR-SCHEMA:FALIAS@ IR--SCHEMA-ALIAS:UNRESTRICTED IR--SCHEMA-ALIAS:EQ ;

: ADDR-SHAPE-CASE ( -- )
   s" the two addressed forms take their base as a value and name no slot" T-LABEL
   BND [: ADDR-SHAPE-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE
   TTRUE TTRUE TTRUE TTRUE 0 T= 1 T= 3 T=
   TTRUE TTRUE TTRUE 0 T= 2 T= 2 T=
   TTRUE TTRUE ;

\ The copy: one register read, one register written, no attribute, no tie, and
\ the same spelling every other reader sees. The absent tie is the whole content
\ of the form - a copy whose two registers were one field would be an
\ instruction that does nothing - so it is asserted here beside the shape.
: MOV-BODY ( IR-CTX:ctx -- bool n n n n bool bool bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   b IR-BUILD:MODULE-KEY {: key:IR-ID:ir-module-key :}
   c b A64IR-OPCODE:MOV A64IR:OPCODE {: v:IR-ID:ir-symbol-id :}
   c b A64IR:GPR-TYPE {: t:IR-ID:ir-type-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FSYM-POOL {: pv:IR-ARENA:view :}
   m IR-BUILD:FSYM-ROWS {: yv:IR-ARENA:view :}
   m IR-BUILD:FSCHEMA-POOL {: qv:IR-ARENA:view :}
   m IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   pv yv v s" a64.mov" IR-SYM:FEQ?
   rv v IR-SCHEMA:FOPERANDS
   rv v IR-SCHEMA:FRESULTS
   rv v IR-SCHEMA:FATTRS
   rv v IR-SCHEMA:FTIES
   rv v IR-SCHEMA:FTERMINATOR?
   rv v IR-SCHEMA:FTRAPS?
   rv v IR-SCHEMA:FEFFECT@ IR--SCHEMA-EFFECT:PURE IR--SCHEMA-EFFECT:EQ ;

: MOV-CASE ( -- )
   s" the copy reads one register, writes one, and ties neither" T-LABEL
   BND [: MOV-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TFALSE TFALSE 0 T= 0 T= 1 T= 1 T= TTRUE ;

\ ---- the comparison and the two branches -------------------------------------
\ The compare form reads two registers, writes the flag, and carries exactly one
\ attribute - the condition. The unconditional branch is a terminator with one
\ successor and a variadic operand tail, which is what makes its operands the
\ destination's block arguments; the two-way branch is a terminator with two
\ successors and exactly one fixed operand, which is what makes it hand nothing
\ over. Both facts are the whole reason an edge that carries values goes through
\ a block of its own, so they are asserted rather than described.
: BRANCH-SHAPE-BODY ( IR-CTX:ctx -- bool bool bool n n n n bool n n n n bool n n n n bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   b IR-BUILD:MODULE-KEY {: key:IR-ID:ir-module-key :}
   c b A64IR-OPCODE:FLAG A64IR:OPCODE {: g:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:BR A64IR:OPCODE {: j:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:BRZ A64IR:OPCODE {: n:IR-ID:ir-symbol-id :}
   c b A64IR:KEY-COND {: ck:IR-ID:ir-symbol-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FSYM-POOL {: pv:IR-ARENA:view :}
   m IR-BUILD:FSYM-ROWS {: yv:IR-ARENA:view :}
   m IR-BUILD:FSCHEMA-POOL {: qv:IR-ARENA:view :}
   m IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   pv yv g s" a64.flag" IR-SYM:FEQ?
   pv yv j s" a64.b" IR-SYM:FEQ?
   pv yv n s" a64.cbz" IR-SYM:FEQ?
   rv g IR-SCHEMA:FOPERANDS
   rv g IR-SCHEMA:FRESULTS
   rv g IR-SCHEMA:FATTRS
   rv g IR-SCHEMA:FSUCCESSORS
   qv rv key g 0 IR-SCHEMA:FATTR@ IR-ID:SYMBOL-LOCAL ck IR-ID:SYMBOL-LOCAL =
   rv j IR-SCHEMA:FOPERANDS
   rv j IR-SCHEMA:FRESULTS
   rv j IR-SCHEMA:FSUCCESSORS
   rv j IR-SCHEMA:FTIES
   rv j IR-SCHEMA:FOPERAND-TAIL?
   rv n IR-SCHEMA:FOPERANDS
   rv n IR-SCHEMA:FRESULTS
   rv n IR-SCHEMA:FSUCCESSORS
   rv n IR-SCHEMA:FATTRS
   rv n IR-SCHEMA:FOPERAND-TAIL? ;

: BRANCH-SHAPE-CASE ( -- )
   s" the comparison and the two branches have the shapes their forms have" T-LABEL
   BND [: BRANCH-SHAPE-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE 0 T= 2 T= 0 T= 1 T=
   TTRUE 0 T= 1 T= 0 T= 1 T=
   TTRUE 0 T= 1 T= 1 T= 2 T=
   TTRUE TTRUE TTRUE ;

\ Every one of the three is a terminator or is not, and none of them traps.
: BRANCH-TERM-BODY ( IR-CTX:ctx -- bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b A64IR-OPCODE:FLAG A64IR:OPCODE {: g:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:BR A64IR:OPCODE {: j:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:BRZ A64IR:OPCODE {: n:IR-ID:ir-symbol-id :}
   c b IR-BUILD:FREEZE IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   rv g IR-SCHEMA:FTERMINATOR?
   rv j IR-SCHEMA:FTERMINATOR?
   rv n IR-SCHEMA:FTERMINATOR?
   rv g IR-SCHEMA:FTRAPS?
   rv j IR-SCHEMA:FTRAPS?
   rv n IR-SCHEMA:FTRAPS? ;

: BRANCH-TERM-CASE ( -- )
   s" the two branches end a block, the comparison does not, and none traps" T-LABEL
   BND [: BRANCH-TERM-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TFALSE TFALSE TTRUE TTRUE TFALSE ;

\ ---- the condition field -----------------------------------------------------
\ The dialect writes the two conditions it compares under as the numbers the
\ four-bit field holds. Those numbers are the assembler's own, so they are read
\ back off src/arch/arm64/asm.f rather than restated here - a condition that
\ moved there reddens this instead of encoding the wrong branch - and the round
\ trip through the stored code is asserted too.
: COND-CASE ( -- )
   s" the conditions are the shipped assembler's, and decode back" T-LABEL
   A64IR-COND:LT A64IR:COND-CODE C-LT T=
   A64IR-COND:LE A64IR:COND-CODE C-LE T=
   A64IR-COND:LT A64IR:COND-CODE A64IR:N>COND A64IR-COND:LT A64IR-COND:EQ TTRUE
   A64IR-COND:LE A64IR:COND-CODE A64IR:N>COND A64IR-COND:LE A64IR-COND:EQ TTRUE ;

\ A code the vocabulary does not name decodes as nothing at all.
: COND-REFUSE-CASES ( -- )
   s" a stored condition outside the vocabulary is refused" T-LABEL
   [: 0 A64IR:N>COND drop ;] catch E-A64IR-COND T=
   [: C-LT 1+ A64IR:N>COND drop ;] catch E-A64IR-COND T= ;

\ ---- the reach of each branch --------------------------------------------------
\ The two displacement fields, at their exact edges. Both encoders mask their
\ field rather than bounding it, so what the emitter asks here is the only thing
\ standing between a branch out of reach and a branch somewhere else.
: REACH-CASE ( -- )
   s" each branch form answers for the field its displacement lands in" T-LABEL
   0 A64IR:B-FITS? TTRUE
   1 25 lshift 1- A64IR:B-FITS? TTRUE
   1 25 lshift A64IR:B-FITS? TFALSE
   1 25 lshift negate A64IR:B-FITS? TTRUE
   1 25 lshift negate 1- A64IR:B-FITS? TFALSE
   0 A64IR:BZ-FITS? TTRUE
   1 18 lshift 1- A64IR:BZ-FITS? TTRUE
   1 18 lshift A64IR:BZ-FITS? TFALSE
   1 18 lshift negate A64IR:BZ-FITS? TTRUE
   1 18 lshift negate 1- A64IR:BZ-FITS? TFALSE ;

\ ---- the tied register field -------------------------------------------------
\ The move-wide overwrite is the one form of this dialect whose result and whose
\ operand are one register field, and its schema is where that is written down.
\ Every other form names each of its registers once, so a consumer that reads the
\ tie gets the constraint from the form instead of from an opcode's name.
: TIE-BODY ( IR-CTX:ctx -- n n n n n n n n n )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b A64IR-OPCODE:MOVZ A64IR:OPCODE {: z:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:MOVK A64IR:OPCODE {: k:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:MOV A64IR:OPCODE {: v:IR-ID:ir-symbol-id :}
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
   rv v IR-SCHEMA:FTIES
   rv z IR-SCHEMA:FTIES
   rv a IR-SCHEMA:FTIES
   rv s IR-SCHEMA:FTIES
   rv u IR-SCHEMA:FTIES
   rv t IR-SCHEMA:FTIES ;

: TIE-CASE ( -- )
   s" the move-wide overwrite declares its one tie and nothing else does" T-LABEL
   BND [: TIE-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 0 T= 0 T= 0 T= 0 T= 0 T= 0 T= 0 T= 1 T= ;

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

\ ---- the data-stack operand refusals -----------------------------------------
\ A data-stack slot the load and store forms cannot address, and an adjustment
\ of the pointer that is not a whole number of cells or does not fit the one
\ immediate that makes it. Both are refused on the production builder, so no
\ module can hold one.
: DSLOT-ODD-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b A64IR:SLOT-WIDTH 1- A64IR:DSLOT-ATTR drop ;

: DSLOT-LOW-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b A64IR:SLOT-WIDTH negate A64IR:DSLOT-ATTR drop ;

: DSLOT-HIGH-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b  A64IR:SLOT-WIDTH A64EFF:SLOT-REACH A64IR:SLOT-WIDTH +  A64IR:DSLOT-ATTR
   drop ;

: DBYTES-ODD-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b 1 A64IR:DBYTES-ATTR drop ;

: DBYTES-LOW-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b A64IR:SLOT-WIDTH negate A64IR:DBYTES-ATTR drop ;

: DBYTES-HIGH-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b  A64IR:FRAME-LIMIT A64EFF:SP-ALIGN +  A64IR:DBYTES-ATTR drop ;

: DSLOT-ODD ( -- )    BND [: DSLOT-ODD-BODY ;] IR-CTX:WITH-CONTEXT ;
: DSLOT-LOW ( -- )    BND [: DSLOT-LOW-BODY ;] IR-CTX:WITH-CONTEXT ;
: DSLOT-HIGH ( -- )   BND [: DSLOT-HIGH-BODY ;] IR-CTX:WITH-CONTEXT ;
: DBYTES-ODD ( -- )   BND [: DBYTES-ODD-BODY ;] IR-CTX:WITH-CONTEXT ;
: DBYTES-LOW ( -- )   BND [: DBYTES-LOW-BODY ;] IR-CTX:WITH-CONTEXT ;
: DBYTES-HIGH ( -- )  BND [: DBYTES-HIGH-BODY ;] IR-CTX:WITH-CONTEXT ;

\ Two refusals per group, for the reason the frame-depth group already gives:
\ each one abandons a context holding a module, and the registry gives those
\ slots back only when a live enclosing context leaves.
: DSLOT-REFUSE-CASES ( -- )
   s" a data-stack slot that is not a whole cell from the pointer is refused" T-LABEL
   [: DSLOT-ODD ;] E-A64IR-DSLOT TTHROWSQ
   s" a negative data-stack slot is refused" T-LABEL
   [: DSLOT-LOW ;] E-A64IR-DSLOT TTHROWSQ ;

: DSLOT-REACH-CASES ( -- )
   s" a data-stack slot past the reach of the offset field is refused" T-LABEL
   [: DSLOT-HIGH ;] E-A64IR-DSLOT TTHROWSQ
   s" a data-stack adjustment that is not whole cells is refused" T-LABEL
   [: DBYTES-ODD ;] E-A64IR-DBYTES TTHROWSQ ;

: DBYTES-REFUSE-CASES ( -- )
   s" a negative data-stack adjustment is refused" T-LABEL
   [: DBYTES-LOW ;] E-A64IR-DBYTES TTHROWSQ
   s" a data-stack adjustment past its immediate is refused" T-LABEL
   [: DBYTES-HIGH ;] E-A64IR-DBYTES TTHROWSQ ;

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

: GROUP-DSLOT-REFUSE ( IR-CTX:ctx -- )
   drop
   DSLOT-REFUSE-CASES ;

: GROUP-DSLOT-REACH ( IR-CTX:ctx -- )
   drop
   DSLOT-REACH-CASES ;

: GROUP-DBYTES-REFUSE ( IR-CTX:ctx -- )
   drop
   DBYTES-REFUSE-CASES ;

: GROUP-MOV ( IR-CTX:ctx -- )
   drop
   MOV-CASE ;

: GROUP-ADDR ( IR-CTX:ctx -- )
   drop
   ADDR-SHAPE-CASE ;

: GROUP-BRANCH ( IR-CTX:ctx -- )
   drop
   BRANCH-SHAPE-CASE
   BRANCH-TERM-CASE ;

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
   COND-CASE
   COND-REFUSE-CASES
   REACH-CASE
   BND [: GROUP-BRANCH ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-REGISTER ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-FRAME-SPELL ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-SHAPE ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-MOV ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-ADDR ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-FRAME-SHAPE ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-TIE ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-IMM-REFUSE ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-SLOT-REFUSE ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-FRAME-REFUSE ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-FRAME-DEPTH ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-DSLOT-REFUSE ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-DSLOT-REACH ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-DBYTES-REFUSE ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-SHIFT-REFUSE ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-TABLE-REFUSE ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-TARGET-REFUSE ;] IR-CTX:WITH-CONTEXT
   T-REPORT ;

;package

A64IR-TEST:RUN

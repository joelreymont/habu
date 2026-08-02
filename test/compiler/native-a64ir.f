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
\ than assumed. The eighteen the register conventions use are here; the four that
\ reach the caller's data stack are checked in DSTACK-SPELL-CASE below, the two
\ addressed cell forms in ADDR-SHAPE-CASE, the two addressed byte forms in
\ BYTE-SHAPE-CASE, the fused compare-and-branch in CMPBR-SHAPE-CASE and the six
\ bitwise and shift forms in BITWISE-CASE, and the count covers all thirty-four.
: COUNT-BODY ( IR-CTX:ctx -- n bool bool bool bool bool bool bool bool bool bool bool bool bool bool bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b A64IR-OPCODE:MOVZ A64IR:OPCODE {: z:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:MOVK A64IR:OPCODE {: k:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:MOV A64IR:OPCODE {: v:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:ADD A64IR:OPCODE {: a:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:SUB A64IR:OPCODE {: s:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:MUL A64IR:OPCODE {: u:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:SDIV A64IR:OPCODE {: y:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:STORE A64IR:OPCODE {: w:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:LOAD A64IR:OPCODE {: d:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:RESERVE A64IR:OPCODE {: p:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:RELEASE A64IR:OPCODE {: q:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:FLAG A64IR:OPCODE {: g:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:BR A64IR:OPCODE {: j:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:BRZ A64IR:OPCODE {: n:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:RET A64IR:OPCODE {: t:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:CALL A64IR:OPCODE {: cl:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:LINKSAVE A64IR:OPCODE {: ls:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:LINKLOAD A64IR:OPCODE {: ll:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:CMPBR A64IR:OPCODE {: cb:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:WORDCALL A64IR:OPCODE {: wc:IR-ID:ir-symbol-id :}
   b IR-BUILD:SCHEMAS
   c b IR-BUILD:FREEZE IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   rv z IR-SCHEMA:FDEFINED?
   rv k IR-SCHEMA:FDEFINED?
   rv v IR-SCHEMA:FDEFINED?
   rv a IR-SCHEMA:FDEFINED?
   rv s IR-SCHEMA:FDEFINED?
   rv u IR-SCHEMA:FDEFINED?
   rv y IR-SCHEMA:FDEFINED?
   rv w IR-SCHEMA:FDEFINED?
   rv d IR-SCHEMA:FDEFINED?
   rv p IR-SCHEMA:FDEFINED?
   rv q IR-SCHEMA:FDEFINED?
   rv g IR-SCHEMA:FDEFINED?
   rv j IR-SCHEMA:FDEFINED?
   rv n IR-SCHEMA:FDEFINED?
   rv t IR-SCHEMA:FDEFINED?
   rv cl IR-SCHEMA:FDEFINED?
   rv ls IR-SCHEMA:FDEFINED?
   rv ll IR-SCHEMA:FDEFINED?
   rv cb IR-SCHEMA:FDEFINED?
   rv wc IR-SCHEMA:FDEFINED? ;

: COUNT-CASE ( -- )
   s" registration defines exactly the forty-nine machine opcodes" T-LABEL
   BND [: COUNT-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE 49 T= ;

\ The six forms the bitwise and shift words lower to. Five are the ordinary
\ two-register three-operand shape and the sixth, the complement, is the one
\ that reads one register and writes one - which is what the operand count read
\ back off its own schema says, and what stops a caller staging it as a
\ two-value operation.
: BITWISE-BODY ( IR-CTX:ctx -- bool bool bool bool bool bool n n )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b A64IR-OPCODE:AND A64IR:OPCODE {: an:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:ORR A64IR:OPCODE {: o:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:EOR A64IR:OPCODE {: x:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:LSLV A64IR:OPCODE {: ls:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:LSRV A64IR:OPCODE {: rs:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:MVN A64IR:OPCODE {: mv:IR-ID:ir-symbol-id :}
   c b IR-BUILD:FREEZE IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   rv an IR-SCHEMA:FDEFINED?
   rv o IR-SCHEMA:FDEFINED?
   rv x IR-SCHEMA:FDEFINED?
   rv ls IR-SCHEMA:FDEFINED?
   rv rs IR-SCHEMA:FDEFINED?
   rv mv IR-SCHEMA:FDEFINED?
   rv an IR-SCHEMA:FOPERANDS
   rv mv IR-SCHEMA:FOPERANDS ;

: BITWISE-CASE ( -- )
   s" the bitwise forms are three-operand and the complement is unary" T-LABEL
   BND [: BITWISE-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 2 T=
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE ;

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
   3 T= 0 T= TTRUE ;

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

\ ---- the declared shapes of the two addressed BYTE forms ---------------------
\ A byte access is its own form, so what has to be asserted is that it is one: a
\ spelling of its own, an opcode identity that is NOT the cell form's, and the
\ same shape the cell form has - address then order in, register then order out
\ for the load; value, address and order in and the order out for the store -
\ with no attribute anywhere, because a width carried as an attribute is exactly
\ what this dialect refuses to do. They are in the generic space with
\ unrestricted aliasing and therefore on the one token chain, because a byte an
\ address names may be a byte of the caller's data stack.
: BYTE-SHAPE-BODY ( IR-CTX:ctx -- bool bool bool n n n bool bool n n n bool bool bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   b IR-BUILD:MODULE-KEY {: key:IR-ID:ir-module-key :}
   c b A64IR-OPCODE:ABLOAD A64IR:OPCODE {: bl:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:ABSTORE A64IR:OPCODE {: bs:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:ALOAD A64IR:OPCODE {: al:IR-ID:ir-symbol-id :}
   c b A64IR:GPR-TYPE {: t:IR-ID:ir-type-id :}
   c b A64IR:MEM-TYPE {: kt:IR-ID:ir-type-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FSYM-POOL {: pv:IR-ARENA:view :}
   m IR-BUILD:FSYM-ROWS {: yv:IR-ARENA:view :}
   m IR-BUILD:FSCHEMA-POOL {: qv:IR-ARENA:view :}
   m IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   pv yv bl s" a64.aldrb" IR-SYM:FEQ?
   pv yv bs s" a64.astrb" IR-SYM:FEQ?
   bl IR-ID:SYMBOL-LOCAL al IR-ID:SYMBOL-LOCAL = 0=
   rv bl IR-SCHEMA:FOPERANDS
   rv bl IR-SCHEMA:FRESULTS
   rv bl IR-SCHEMA:FATTRS
   qv rv key bl 0 IR-SCHEMA:FOPERAND@ IR-ID:TYPE-LOCAL t IR-ID:TYPE-LOCAL =
   qv rv key bl 1 IR-SCHEMA:FOPERAND@ IR-ID:TYPE-LOCAL kt IR-ID:TYPE-LOCAL =
   rv bs IR-SCHEMA:FOPERANDS
   rv bs IR-SCHEMA:FRESULTS
   rv bs IR-SCHEMA:FATTRS
   rv bl IR-SCHEMA:FEFFECT@ IR--SCHEMA-EFFECT:READ IR--SCHEMA-EFFECT:EQ
   rv bs IR-SCHEMA:FEFFECT@ IR--SCHEMA-EFFECT:WRITE IR--SCHEMA-EFFECT:EQ
   rv bs IR-SCHEMA:FALIAS@ IR--SCHEMA-ALIAS:UNRESTRICTED IR--SCHEMA-ALIAS:EQ ;

: BYTE-SHAPE-CASE ( -- )
   s" the two byte forms are forms of their own with the addressed shape" T-LABEL
   BND [: BYTE-SHAPE-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE
   0 T= 1 T= 3 T=
   TTRUE TTRUE 0 T= 2 T= 2 T=
   TTRUE TTRUE TTRUE ;

\ The division: two registers read, one written, no attribute - the same shape
\ as the three arithmetic forms - and the ONE form of this dialect whose schema
\ says it may raise. Its three instructions are the zero-divisor guard and the
\ divide together, which is what makes the raise real: a compiled division ends
\ the process where the engine's own `/` does instead of answering zero. The
\ multiply is asserted beside it as total, so the trap flag is a statement about
\ division rather than about arithmetic.
: SDIV-BODY ( IR-CTX:ctx -- bool n n n bool bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b A64IR-OPCODE:SDIV A64IR:OPCODE {: y:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:MUL A64IR:OPCODE {: u:IR-ID:ir-symbol-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FSYM-POOL {: pv:IR-ARENA:view :}
   m IR-BUILD:FSYM-ROWS {: yv:IR-ARENA:view :}
   m IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   pv yv y s" a64.sdiv" IR-SYM:FEQ?
   rv y IR-SCHEMA:FOPERANDS
   rv y IR-SCHEMA:FRESULTS
   rv y IR-SCHEMA:FATTRS
   rv y IR-SCHEMA:FTRAPS?
   rv u IR-SCHEMA:FTRAPS? ;

: SDIV-CASE ( -- )
   s" the division is the one machine form that may raise" T-LABEL
   BND [: SDIV-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TTRUE 0 T= 1 T= 2 T= TTRUE ;

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

\ ---- the fused compare-and-branch --------------------------------------------
\ It is the shape of BOTH the forms it replaces at once: two register operands
\ like the comparison, one condition attribute like the comparison, two
\ successors like the two-way branch - and NO result, which is the whole saving.
\ A result here would mean the flag was materialised after all, and a successor
\ count of one would mean an edge that has to carry values could reach it.
\ It ends a block and it does not trap.
: CMPBR-SHAPE-BODY ( IR-CTX:ctx -- bool n n n n bool bool bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   b IR-BUILD:MODULE-KEY {: key:IR-ID:ir-module-key :}
   c b A64IR-OPCODE:CMPBR A64IR:OPCODE {: n:IR-ID:ir-symbol-id :}
   c b A64IR:KEY-COND {: ck:IR-ID:ir-symbol-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FSYM-POOL {: pv:IR-ARENA:view :}
   m IR-BUILD:FSYM-ROWS {: yv:IR-ARENA:view :}
   m IR-BUILD:FSCHEMA-POOL {: qv:IR-ARENA:view :}
   m IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   pv yv n s" a64.cmpbr" IR-SYM:FEQ?
   rv n IR-SCHEMA:FOPERANDS
   rv n IR-SCHEMA:FRESULTS
   rv n IR-SCHEMA:FSUCCESSORS
   rv n IR-SCHEMA:FATTRS
   qv rv key n 0 IR-SCHEMA:FATTR@ IR-ID:SYMBOL-LOCAL ck IR-ID:SYMBOL-LOCAL =
   rv n IR-SCHEMA:FTERMINATOR?
   rv n IR-SCHEMA:FTRAPS? ;

: CMPBR-SHAPE-CASE ( -- )
   s" the fused compare-and-branch compares two registers and defines none"
   T-LABEL
   BND [: CMPBR-SHAPE-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TTRUE TTRUE 1 T= 2 T= 0 T= 2 T= TTRUE ;

\ ---- the four float comparison forms -----------------------------------------
\ Their shapes, and the one thing that is genuinely new in them: the two register
\ classes meet inside one operation. A materialising float comparison reads
\ DOUBLES and writes a CELL, because a Habu flag is a number and lives in the
\ general file whichever file the values compared came out of - a form that
\ answered a double would leave the flag where no branch of this machine can read
\ it. The fused pair answers nothing at all, which is the saving, and each of
\ them carries the condition under the same key the integer forms use.
\
\ AND THE ZERO FORMS TAKE ONE OPERAND, which is the other thing asserted here.
\ FCMP against the immediate zero is a real form of the instruction and the
\ engine's own `f0<` uses it, so the operand list is one long; a schema with two
\ would oblige every lowering to materialise a zero the instruction never reads.
: FCMP-SHAPE-BODY ( IR-CTX:ctx -- bool bool bool bool n n n n n n n n bool bool bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   b IR-BUILD:MODULE-KEY {: key:IR-ID:ir-module-key :}
   c b A64IR-OPCODE:FFLAG A64IR:OPCODE {: g:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:FFLAGZ A64IR:OPCODE {: gz:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:FCMPBR A64IR:OPCODE {: n:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:FCMPBRZ A64IR:OPCODE {: nz:IR-ID:ir-symbol-id :}
   c b A64IR:KEY-COND {: ck:IR-ID:ir-symbol-id :}
   c b A64IR:GPR-TYPE {: t:IR-ID:ir-type-id :}
   c b A64IR:FPR-TYPE {: f:IR-ID:ir-type-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FSYM-POOL {: pv:IR-ARENA:view :}
   m IR-BUILD:FSYM-ROWS {: yv:IR-ARENA:view :}
   m IR-BUILD:FSCHEMA-POOL {: qv:IR-ARENA:view :}
   m IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   pv yv g s" a64.fflag" IR-SYM:FEQ?
   pv yv gz s" a64.fflagz" IR-SYM:FEQ?
   pv yv n s" a64.fcmpbr" IR-SYM:FEQ?
   pv yv nz s" a64.fcmpbrz" IR-SYM:FEQ?
   rv g IR-SCHEMA:FOPERANDS
   rv g IR-SCHEMA:FRESULTS
   rv gz IR-SCHEMA:FOPERANDS
   rv gz IR-SCHEMA:FRESULTS
   rv n IR-SCHEMA:FOPERANDS
   rv n IR-SCHEMA:FRESULTS
   rv nz IR-SCHEMA:FOPERANDS
   rv nz IR-SCHEMA:FRESULTS
   qv rv key g 0 IR-SCHEMA:FOPERAND@ IR-ID:TYPE-LOCAL f IR-ID:TYPE-LOCAL =
   qv rv key g 1 IR-SCHEMA:FOPERAND@ IR-ID:TYPE-LOCAL f IR-ID:TYPE-LOCAL =
   qv rv key g 0 IR-SCHEMA:FRESULT@ IR-ID:TYPE-LOCAL t IR-ID:TYPE-LOCAL =
   qv rv key gz 0 IR-SCHEMA:FOPERAND@ IR-ID:TYPE-LOCAL f IR-ID:TYPE-LOCAL =
   qv rv key gz 0 IR-SCHEMA:FRESULT@ IR-ID:TYPE-LOCAL t IR-ID:TYPE-LOCAL =
   qv rv key n 0 IR-SCHEMA:FOPERAND@ IR-ID:TYPE-LOCAL f IR-ID:TYPE-LOCAL =
   qv rv key n 0 IR-SCHEMA:FATTR@ IR-ID:SYMBOL-LOCAL ck IR-ID:SYMBOL-LOCAL =
   qv rv key nz 0 IR-SCHEMA:FATTR@ IR-ID:SYMBOL-LOCAL ck IR-ID:SYMBOL-LOCAL = ;

: FCMP-SHAPE-CASE ( -- )
   s" the float comparisons read doubles, answer a cell, and the zero forms take one" T-LABEL
   BND [: FCMP-SHAPE-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE
   0 T= 1 T= 0 T= 2 T=
   1 T= 1 T= 1 T= 2 T=
   TTRUE TTRUE TTRUE TTRUE ;

\ Which of them ends a block, and that none of them traps. A comparison against a
\ NaN answers false rather than raising, so the trap flag is false everywhere -
\ and the two fused forms are terminators while the two materialising ones are
\ not, which is what stops a fused branch from standing anywhere but last.
: FCMP-TERM-BODY ( IR-CTX:ctx -- bool bool bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b A64IR-OPCODE:FFLAG A64IR:OPCODE {: g:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:FFLAGZ A64IR:OPCODE {: gz:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:FCMPBR A64IR:OPCODE {: n:IR-ID:ir-symbol-id :}
   c b A64IR-OPCODE:FCMPBRZ A64IR:OPCODE {: nz:IR-ID:ir-symbol-id :}
   c b IR-BUILD:FREEZE IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   rv g IR-SCHEMA:FTERMINATOR?
   rv gz IR-SCHEMA:FTERMINATOR?
   rv n IR-SCHEMA:FTERMINATOR?
   rv nz IR-SCHEMA:FTERMINATOR?
   rv g IR-SCHEMA:FTRAPS?
   rv gz IR-SCHEMA:FTRAPS?
   rv n IR-SCHEMA:FTRAPS?
   rv nz IR-SCHEMA:FTRAPS? ;

: FCMP-TERM-CASE ( -- )
   s" the two fused forms end a block, the two materialising ones do not, none traps" T-LABEL
   BND [: FCMP-TERM-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TFALSE TFALSE TFALSE TTRUE TTRUE TFALSE TFALSE ;

\ ---- the condition a float comparison is made under --------------------------
\ THE ONE FACT THIS WHOLE LEAF RESTS ON, held as a number rather than described.
\ `mi` is C-MI, the assembler's own code for it, and it is NOT `lt`. The
\ difference decides what a compiled float comparison answers for a NaN: an Fcmp
\ raises the unordered condition (N=0 Z=0 C=1 V=1), under which `lt` - whose test
\ is N != V - HOLDS, while `mi` - whose test is N = 1 - does not. The engine's
\ own `f<` uses C-MI (src/habu/habu1.f BF<), which is why the compiled word can
\ agree with the interpreted one at all.
\
\ The three conditions the float words reach are asserted to be exactly the three
\ the engine's primitives name, read off the ENGINE'S constants rather than
\ restated: if habu1.f ever compiled `f<` under another condition, this reddens.
\ The three that are NOT reachable from a float word - `lt`, `le` and `ne` - are
\ asserted to differ from all three, which is the falsification: a table that had
\ quietly lowered `f<` under `lt` would make the first of those equalities hold.
: FCOND-CASE ( -- )
   s" the float conditions are the engine's own, and `mi` is not `lt`" T-LABEL
   A64IR-COND:MI A64IR:COND-CODE C-MI T=
   A64IR-COND:MI A64IR:COND-CODE A64IR:N>COND A64IR-COND:MI A64IR-COND:EQ TTRUE
   A64IR-COND:MI A64IR:COND-CODE  A64IR-COND:LT A64IR:COND-CODE  = TFALSE
   A64IR-COND:MI A64IR:COND-CODE  A64IR-COND:LE A64IR:COND-CODE  = TFALSE
   A64IR-COND:GT A64IR:COND-CODE  A64IR-COND:NE A64IR:COND-CODE  = TFALSE
   A64IR-COND:EQUAL A64IR:COND-CODE  A64IR-COND:NE A64IR:COND-CODE  = TFALSE ;

\ ---- the two float compare encoders ------------------------------------------
\ Both are forms of Fcmp that the instruction parity gate's 48-form vocabulary
\ does not model, exactly as `a64.mvn` is not modelled, so their bits are held
\ here against the shipped assembler until a model row lands. What is asserted is
\ the absolute word - `fcmp d1, d3` is 0x1E632020 and `fcmp d1, #0.0` is
\ 0x1E602028 - and the ONE bit that separates them, which is bit 3: the
\ compare-with-zero form is the register form with that bit set and its second
\ register field empty. Asserting the relation as well as the words is what says
\ the zero form really is the zero form rather than a compare against d0.
: FCMP-ENC-CASE ( -- )
   s" the two Fcmp forms encode the words they are, and differ in the zero bit" T-LABEL
   1 3 ENC-FCMP $1E632020 T=
   1 ENC-FCMP0 $1E602028 T=
   1 ENC-FCMP0  1 0 ENC-FCMP 8 or  T=
   1 0 ENC-FCMP  1 ENC-FCMP0  = TFALSE ;

\ ---- the one encoder this dialect brought with it ----------------------------
\ Every other machine form of this dialect encodes through a word the instruction
\ parity gate already pins against formal/Common/Insn.v. `a64.mvn` does not: its
\ Orn base was added to src/arch/arm64/asm.f with this dialect's complement and
\ is not in that model yet (dot habu-model-orn-in-39435de5). So the encoding is
\ held here, three ways, until the model row lands.
\
\ THE ABSOLUTE WORD is what says the bits are right at all: `mvn x1, x3` is
\ 0xAA2303E1 and nothing else. THE RELATION TO Orr is what says WHICH bit makes
\ it a complement - the shifted-register N bit, at 21 - and it is asserted
\ against ENC-ORR's own output rather than against a second constant, so a base
\ that moved reddens here instead of encoding some other instruction. AND THE
\ ZERO-REGISTER IDENTITY is what says the complement really is the Orn form with
\ the zero register as its first source, which is the whole of what ENC-MVN
\ claims to be - the same shape ENC-MOV has over ENC-ORR.
: ORN-CASE ( -- )
   s" the complement encoder is Orn with the zero register" T-LABEL
   1 3 ENC-MVN $AA2303E1 T=
   1 2 3 ENC-ORN  1 2 3 ENC-ORR  1 21 lshift or  T=
   1 3 ENC-MVN  1 31 3 ENC-ORN  T=
   1 3 ENC-MOV  1 31 3 ENC-ORR  T= ;

\ ---- the condition field -----------------------------------------------------
\ The dialect writes the six conditions its comparisons are made under as the
\ numbers the four-bit field holds. Those numbers are the assembler's own, so
\ they are read back off src/arch/arm64/asm.f rather than restated here - a
\ condition that moved there reddens this instead of encoding the wrong branch -
\ and the round trip through the stored code is asserted too.
\
\ THE THREE COMPLEMENTS ARE HELD AGAINST THEIR OWN NAMES AND NOT AGAINST EACH
\ OTHER. `gt` is C-GT and not "C-LE with something turned round": a lowering
\ that reached greater-than by swapping the compare's operands would still pass
\ a check written the second way, and would encode a different instruction.
: COND-CASE ( -- )
   s" the conditions are the shipped assembler's, and decode back" T-LABEL
   A64IR-COND:LT A64IR:COND-CODE C-LT T=
   A64IR-COND:LE A64IR:COND-CODE C-LE T=
   A64IR-COND:GT A64IR:COND-CODE C-GT T=
   A64IR-COND:GE A64IR:COND-CODE C-GE T=
   A64IR-COND:LT A64IR:COND-CODE A64IR:N>COND A64IR-COND:LT A64IR-COND:EQ TTRUE
   A64IR-COND:LE A64IR:COND-CODE A64IR:N>COND A64IR-COND:LE A64IR-COND:EQ TTRUE
   A64IR-COND:GT A64IR:COND-CODE A64IR:N>COND A64IR-COND:GT A64IR-COND:EQ TTRUE
   A64IR-COND:GE A64IR:COND-CODE A64IR:N>COND A64IR-COND:GE A64IR-COND:EQ TTRUE
   A64IR-COND:NE A64IR:COND-CODE C-NE T=
   A64IR-COND:NE A64IR:COND-CODE A64IR:N>COND A64IR-COND:NE A64IR-COND:EQ TTRUE
   A64IR-COND:EQUAL A64IR:COND-CODE C-EQ T=
   A64IR-COND:EQUAL A64IR:COND-CODE A64IR:N>COND A64IR-COND:EQUAL A64IR-COND:EQ
   TTRUE ;

\ A code the vocabulary does not name decodes as nothing at all. The two chosen
\ are conditions the machine really has - carry set, and always - so what is
\ being refused is a code outside this DIALECT's vocabulary rather than a number
\ outside the field.
: COND-REFUSE-CASES ( -- )
   s" a stored condition outside the vocabulary is refused" T-LABEL
   [: C-CS A64IR:N>COND drop ;] catch E-A64IR-COND T=
   [: C-AL A64IR:N>COND drop ;] catch E-A64IR-COND T= ;

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
   1 18 lshift negate 1- A64IR:BZ-FITS? TFALSE
   0 A64IR:BCOND-FITS? TTRUE
   1 18 lshift 1- A64IR:BCOND-FITS? TTRUE
   1 18 lshift A64IR:BCOND-FITS? TFALSE
   1 18 lshift negate A64IR:BCOND-FITS? TTRUE
   1 18 lshift negate 1- A64IR:BCOND-FITS? TFALSE ;

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

\ ---- the callee entry a call to another word carries -------------------------
\ An address of CODE on this machine: the address of a whole instruction, and
\ not the null address, where no code lives. How FAR away it is is deliberately
\ NOT asked here - the distance depends on where the calling routine is written,
\ which nothing before emission knows - so a legal entry below is one this
\ dialect accepts and the emitter may still refuse for reach.
: ENTRY-ODD-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b $4002 A64IR:ENTRY-ATTR drop ;

: ENTRY-NULL-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b 0 A64IR:ENTRY-ATTR drop ;

: ENTRY-ODD ( -- )   BND [: ENTRY-ODD-BODY ;] IR-CTX:WITH-CONTEXT ;
: ENTRY-NULL ( -- )  BND [: ENTRY-NULL-BODY ;] IR-CTX:WITH-CONTEXT ;

: ENTRY-REFUSE-CASES ( -- )
   s" a callee address that is no whole instruction is refused" T-LABEL
   [: ENTRY-ODD ;] E-A64IR-ENTRY TTHROWSQ
   s" a callee at the null address is refused" T-LABEL
   [: ENTRY-NULL ;] E-A64IR-ENTRY TTHROWSQ ;

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
   BITWISE-CASE
   ORN-CASE
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

: GROUP-ENTRY-REFUSE ( IR-CTX:ctx -- )
   drop
   ENTRY-REFUSE-CASES ;

: GROUP-MOV ( IR-CTX:ctx -- )
   drop
   MOV-CASE ;

: GROUP-ADDR ( IR-CTX:ctx -- )
   drop
   ADDR-SHAPE-CASE
   BYTE-SHAPE-CASE
   SDIV-CASE ;

: GROUP-BRANCH ( IR-CTX:ctx -- )
   drop
   BRANCH-SHAPE-CASE
   BRANCH-TERM-CASE
   CMPBR-SHAPE-CASE ;

: GROUP-FCMP ( IR-CTX:ctx -- )
   drop
   FCMP-SHAPE-CASE
   FCMP-TERM-CASE ;

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
   FCOND-CASE
   FCMP-ENC-CASE
   COND-REFUSE-CASES
   REACH-CASE
   BND [: GROUP-BRANCH ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-FCMP ;] IR-CTX:WITH-CONTEXT
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
   BND [: GROUP-ENTRY-REFUSE ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-SHIFT-REFUSE ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-TABLE-REFUSE ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-TARGET-REFUSE ;] IR-CTX:WITH-CONTEXT
   T-REPORT ;

;package

A64IR-TEST:RUN

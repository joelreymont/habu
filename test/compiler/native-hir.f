\ native-hir.f - checked straight-line HIR dialect tests: policy-dependent
\ schemas, source-word ownership, memoization, and refusals ordinary compilation
\ cannot construct (forged rows, swapped arenas, foreign symbols, full tables,
\ invalid stages, out-of-range picks). native-vocab.f and native-rename-rows.f
\ cover arithmetic, control words and stack renames through load-and-execute.
\ Tier-neutral: every row is read back through the dialect's own API.

require src/lib/test.f
require src/compiler/ir/symbol.f
require src/compiler/native/hir-word.f
require src/compiler/native/feed.f
require src/compiler/native/abi.f

\ ---- a table of its own module asks the binding gate too ----------------------
\ A table built with HIR-WORD:NEW has no session link and a row reader has no
\ interner, so the binding gate is asked when the row is written: a spelling a
\ package has taken over gets no row, and the token reaches that package's word.
\ This package owns `xor` and registers inside itself, so its table must model
\ `and` and not `xor`; registering all 87 rows unconditionally models both.
package HIR-BOUND-XOR
public

: xor ( n n -- n ) drop drop 99 ;

private

variable OWN-AND
variable OWN-XOR

: OWN-BODY ( IR-CTX:ctx -- bool bool )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN IR-BUILD:PLAN-DEFAULT
   c s" habu" 1 0 IR-BUILD:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b IR-BUILD:MODULE-KEY HIR-WORD:WORDS HIR-WORD:PICK-CELLS HIR-WORD:NEW
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   c b p r HIR-WORD:REGISTER-WORDS
   r  c b s" and" IR-BUILD:INTERN-SYMBOL  HIR-WORD:MODELS?
   r  c b s" xor" IR-BUILD:INTERN-SYMBOL  HIR-WORD:MODELS? ;

: RECORD ( -- )
   NABI:BINDING [: OWN-BODY ;] IR-CTX:WITH-CONTEXT
   {: a:bool x:bool :}
   x if 1 else 0 then OWN-XOR !
   a if 1 else 0 then OWN-AND ! ;
RECORD

public

: AND-MODELED? ( -- bool ) OWN-AND @ 0<> ;
: XOR-MODELED? ( -- bool ) OWN-XOR @ 0<> ;

;package

package HIR-TEST
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

\ The same machine with a wrapping overflow policy.
: WBND ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:WRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ The same machine without a floating-point unit: the integer subset registers,
\ the float subset declares the floating feature and must not.
: NOFP-BND ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:CONTRACT
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ Shared HIR can describe a GPU target; A64IR still rejects that binding.
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
   c HIR:NEW-BUILDER ;

: DIALECT-NEW ( IR-CTX:ctx -- IR-BUILD:builder )
   {: c:IR-CTX:ctx :}
   c MOD-NEW {: b:IR-BUILD:builder :}
   c b HIR:REGISTER
   b ;

: OPCODE-NTH-LOW ( -- )
   -1 HIR:NTH drop ;

: OPCODE-NTH-HIGH ( -- )
   HIR:OPCODES HIR:NTH drop ;

: OPCODE-ORDINAL-CASE ( -- )
   s" every HIR opcode round-trips through the dialect-owned ordinal" T-LABEL
   HIR:OPCODES 0 ?do
      i HIR:NTH HIR:ORD i T=
   loop
   s" the stored HIR ordinal is independent of enum declaration order" T-LABEL
   HIR-OPCODE:RETURN HIR:ORD 4 T=
   HIR-OPCODE:QUOT HIR:ORD 45 T=
   HIR-OPCODE:TERMINAL HIR:ORD 46 T=
   s" HIR opcode ordinals below the vocabulary are refused" T-LABEL
   [: OPCODE-NTH-LOW ;] E-HIR-OPCODE TTHROWSQ
   s" HIR opcode ordinals above the vocabulary are refused" T-LABEL
   [: OPCODE-NTH-HIGH ;] E-HIR-OPCODE TTHROWSQ ;

: ARITH-BODY ( IR-CTX:ctx -- n bool n bool n n n bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   b IR-BUILD:MODULE-KEY {: key:IR-ID:ir-module-key :}
   c b HIR-OPCODE:ADD HIR:OPCODE {: op:IR-ID:ir-symbol-id :}
   c b IR--TYPE-WIDTH:W64 IR--TYPE-SIGN:SIGNED IR-BUILD:INTERN-INT {: t:IR-ID:ir-type-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FSCHEMA-POOL {: qv:IR-ARENA:view :}
   m IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   rv op IR-SCHEMA:FOPERANDS
   rv op IR-SCHEMA:FOPERAND-TAIL?
   rv op IR-SCHEMA:FRESULTS
   rv op IR-SCHEMA:FRESULT-TAIL?
   rv op IR-SCHEMA:FSUCCESSORS
   rv op IR-SCHEMA:FREGIONS
   rv op IR-SCHEMA:FATTRS
   rv op IR-SCHEMA:FATTR-EXT?
   rv op IR-SCHEMA:FTERMINATOR?
   rv op IR-SCHEMA:FEFFECT@ IR--SCHEMA-EFFECT:PURE IR--SCHEMA-EFFECT:EQ
   rv op IR-SCHEMA:FARCH@ CTARGET-ARCH:AARCH64 CTARGET-ARCH:EQ
   qv rv key op 0 IR-SCHEMA:FOPERAND@ IR-ID:TYPE-LOCAL t IR-ID:TYPE-LOCAL =
   qv rv key op 0 IR-SCHEMA:FRESULT@ IR-ID:TYPE-LOCAL t IR-ID:TYPE-LOCAL = ;

: ARITH-CASE ( -- )
   s" an arithmetic opcode reads back exactly as declared" T-LABEL
   BND [: ARITH-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE
   TFALSE TFALSE 0 T= 0 T= 0 T= TFALSE 1 T= TFALSE 2 T= ;

: SHAPE-BODY ( IR-CTX:ctx -- n n n bool bool bool n n bool bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   b IR-BUILD:MODULE-KEY {: key:IR-ID:ir-module-key :}
   c b HIR-OPCODE:CONST HIR:OPCODE {: k:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:RETURN HIR:OPCODE {: r:IR-ID:ir-symbol-id :}
   c b HIR:KEY-VALUE {: vk:IR-ID:ir-symbol-id :}
   c b HIR:KEY-ADDR {: ak:IR-ID:ir-symbol-id :}
   c b HIR:KEY-ORIGIN {: ok:IR-ID:ir-symbol-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FSCHEMA-POOL {: qv:IR-ARENA:view :}
   m IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   rv k IR-SCHEMA:FOPERANDS
   rv k IR-SCHEMA:FRESULTS
   rv k IR-SCHEMA:FATTRS
   qv rv key k 0 IR-SCHEMA:FATTR@ IR-ID:SYMBOL-LOCAL vk IR-ID:SYMBOL-LOCAL =
   qv rv key k 1 IR-SCHEMA:FATTR@ IR-ID:SYMBOL-LOCAL ak IR-ID:SYMBOL-LOCAL =
   qv rv key k 2 IR-SCHEMA:FATTR@ IR-ID:SYMBOL-LOCAL ok IR-ID:SYMBOL-LOCAL =
   rv r IR-SCHEMA:FOPERANDS
   rv r IR-SCHEMA:FRESULTS
   rv r IR-SCHEMA:FOPERAND-TAIL?
   rv r IR-SCHEMA:FTERMINATOR? ;

\ The constant's attribute keys in schema order: number, address class, DATA
\ origin. The ordinals are pinned: a reader that swapped ordinals 0 and 1 would
\ materialise the kind as the literal.
: SHAPE-CASE ( -- )
   s" the constant and the return have the shapes section 7.2 gives them" T-LABEL
   BND [: SHAPE-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE 0 T= 1 T= TTRUE TTRUE TTRUE 3 T= 1 T= 0 T= ;

\ Memory schema types are checked by position, and both accesses declare a
\ memory effect, not purity: that makes the freeze verifier demand their order.
: MEM-SHAPE-BODY ( IR-CTX:ctx -- n n bool n n bool bool bool n n bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   b IR-BUILD:MODULE-KEY {: key:IR-ID:ir-module-key :}
   c b HIR-OPCODE:MEM HIR:OPCODE {: mk:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:LOAD HIR:OPCODE {: ld:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:STORE HIR:OPCODE {: st:IR-ID:ir-symbol-id :}
   c b IR--TYPE-WIDTH:W64 IR--TYPE-SIGN:SIGNED IR-BUILD:INTERN-INT
   {: t:IR-ID:ir-type-id :}
   c b HIR:MEM-TYPE {: kt:IR-ID:ir-type-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FSCHEMA-POOL {: qv:IR-ARENA:view :}
   m IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   rv mk IR-SCHEMA:FOPERANDS
   rv mk IR-SCHEMA:FRESULTS
   rv mk IR-SCHEMA:FEFFECT@ IR--SCHEMA-EFFECT:PURE IR--SCHEMA-EFFECT:EQ
   rv ld IR-SCHEMA:FOPERANDS
   rv ld IR-SCHEMA:FRESULTS
   qv rv key ld 0 IR-SCHEMA:FOPERAND@ IR-ID:TYPE-LOCAL t IR-ID:TYPE-LOCAL =
   qv rv key ld 1 IR-SCHEMA:FOPERAND@ IR-ID:TYPE-LOCAL kt IR-ID:TYPE-LOCAL =
   rv ld IR-SCHEMA:FEFFECT@ IR--SCHEMA-EFFECT:READ IR--SCHEMA-EFFECT:EQ
   rv st IR-SCHEMA:FOPERANDS
   rv st IR-SCHEMA:FRESULTS
   qv rv key st 0 IR-SCHEMA:FOPERAND@ IR-ID:TYPE-LOCAL t IR-ID:TYPE-LOCAL =
   qv rv key st 1 IR-SCHEMA:FOPERAND@ IR-ID:TYPE-LOCAL t IR-ID:TYPE-LOCAL =
   qv rv key st 2 IR-SCHEMA:FOPERAND@ IR-ID:TYPE-LOCAL kt IR-ID:TYPE-LOCAL =
   rv st IR-SCHEMA:FEFFECT@ IR--SCHEMA-EFFECT:WRITE IR--SCHEMA-EFFECT:EQ
   rv st IR-SCHEMA:FALIAS@ IR--SCHEMA-ALIAS:UNRESTRICTED IR--SCHEMA-ALIAS:EQ ;

: MEM-SHAPE-CASE ( -- )
   s" the memory schemas carry an address, a value and one order" T-LABEL
   BND [: MEM-SHAPE-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE 1 T= 3 T=
   TTRUE TTRUE TTRUE 2 T= 2 T=
   TTRUE 1 T= 0 T= ;

\ ---- the trap flag is the unit's policy --------------------------------------
\ Registered under a trapping policy the three arithmetic opcodes may trap;
\ under a wrapping one they may not. The constant and the return never do.
: TRAP-BODY ( IR-CTX:ctx -- bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b HIR-OPCODE:ADD HIR:OPCODE {: a:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:SUB HIR:OPCODE {: s:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:MUL HIR:OPCODE {: u:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:CONST HIR:OPCODE {: k:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:RETURN HIR:OPCODE {: r:IR-ID:ir-symbol-id :}
   c b IR-BUILD:FREEZE IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   rv a IR-SCHEMA:FTRAPS?
   rv s IR-SCHEMA:FTRAPS?
   rv u IR-SCHEMA:FTRAPS?
   rv k IR-SCHEMA:FTRAPS?
   rv r IR-SCHEMA:FTRAPS? ;

: TRAP-CASES ( -- )
   s" a trapping overflow policy makes the arithmetic opcodes may-trap" T-LABEL
   BND [: TRAP-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TFALSE TTRUE TTRUE TTRUE
   s" a wrapping overflow policy makes the same opcodes total" T-LABEL
   WBND [: TRAP-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TFALSE TFALSE TFALSE TFALSE ;

\ ---- the one opcode whose trap is not the unit's policy -----------------------
\ Division may trap under either policy: it traps on a zero divisor, not an
\ overflow. The engine's `/` branches over a `brk` when the divisor is not zero,
\ so a lowering that dropped the check would answer zero where the interpreted
\ word ends the process.
: DIV-TRAP-BODY ( IR-CTX:ctx -- bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b HIR-OPCODE:DIV HIR:OPCODE {: v:IR-ID:ir-symbol-id :}
   c b IR-BUILD:FREEZE IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   rv v IR-SCHEMA:FTRAPS? ;

: DIV-TRAP-CASES ( -- )
   s" division may trap under a trapping policy" T-LABEL
   BND [: DIV-TRAP-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE
   s" and under a wrapping one too, because it is not an overflow" T-LABEL
   WBND [: DIV-TRAP-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE ;

\ ---- the float schemas -------------------------------------------------------
\ Float schemas are asserted by type at each position: a schema naming the cell
\ type at any of them would let a double reach a general register unrefused.
: FSHAPE-BODY ( IR-CTX:ctx -- n n bool bool bool bool bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   b IR-BUILD:MODULE-KEY {: key:IR-ID:ir-module-key :}
   c b HIR-OPCODE:FADD HIR:OPCODE {: fa:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:FSQRT HIR:OPCODE {: fq:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:INTREAL HIR:OPCODE {: ir:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:REALINT HIR:OPCODE {: ri:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:BITSREAL HIR:OPCODE {: br:IR-ID:ir-symbol-id :}
   c b IR--TYPE-WIDTH:W64 IR--TYPE-SIGN:SIGNED IR-BUILD:INTERN-INT
   {: t:IR-ID:ir-type-id :}
   c b HIR:REAL-TYPE {: f:IR-ID:ir-type-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FSCHEMA-POOL {: qv:IR-ARENA:view :}
   m IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   rv fa IR-SCHEMA:FOPERANDS
   rv fa IR-SCHEMA:FRESULTS
   qv rv key fa 0 IR-SCHEMA:FOPERAND@ IR-ID:TYPE-LOCAL f IR-ID:TYPE-LOCAL =
   qv rv key fa 1 IR-SCHEMA:FOPERAND@ IR-ID:TYPE-LOCAL f IR-ID:TYPE-LOCAL =
   qv rv key fa 0 IR-SCHEMA:FRESULT@ IR-ID:TYPE-LOCAL f IR-ID:TYPE-LOCAL =
   qv rv key fq 0 IR-SCHEMA:FOPERAND@ IR-ID:TYPE-LOCAL f IR-ID:TYPE-LOCAL =
   qv rv key ir 0 IR-SCHEMA:FOPERAND@ IR-ID:TYPE-LOCAL t IR-ID:TYPE-LOCAL =
   qv rv key ir 0 IR-SCHEMA:FRESULT@ IR-ID:TYPE-LOCAL f IR-ID:TYPE-LOCAL =
   qv rv key ri 0 IR-SCHEMA:FOPERAND@ IR-ID:TYPE-LOCAL f IR-ID:TYPE-LOCAL =
   qv rv key ri 0 IR-SCHEMA:FRESULT@ IR-ID:TYPE-LOCAL t IR-ID:TYPE-LOCAL =
   qv rv key br 0 IR-SCHEMA:FRESULT@ IR-ID:TYPE-LOCAL f IR-ID:TYPE-LOCAL =
   rv fa IR-SCHEMA:FFEATURES@ CTARGET:F-SCALAR-FP CTARGET:HAS? ;

: FSHAPE-CASE ( -- )
   s" the float opcodes carry the double type at every position, and need the unit" T-LABEL
   BND [: FSHAPE-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE
   1 T= 2 T= ;

\ Float comparisons take doubles and answer a cell (a Habu flag; no branch reads
\ a float register). The two against zero take one operand: the instruction
\ compares against an immediate the form carries.
: FCMP-SHAPE-BODY ( IR-CTX:ctx -- n n n bool bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   b IR-BUILD:MODULE-KEY {: key:IR-ID:ir-module-key :}
   c b HIR-OPCODE:FLT HIR:OPCODE {: fl:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:FGT HIR:OPCODE {: fg:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:FEQ HIR:OPCODE {: fe:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:FLTZ HIR:OPCODE {: fz:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:FEQZ HIR:OPCODE {: ez:IR-ID:ir-symbol-id :}
   c b IR--TYPE-WIDTH:W64 IR--TYPE-SIGN:SIGNED IR-BUILD:INTERN-INT
   {: t:IR-ID:ir-type-id :}
   c b HIR:REAL-TYPE {: f:IR-ID:ir-type-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FSCHEMA-POOL {: qv:IR-ARENA:view :}
   m IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   rv fl IR-SCHEMA:FOPERANDS
   rv fz IR-SCHEMA:FOPERANDS
   rv fl IR-SCHEMA:FRESULTS
   qv rv key fl 0 IR-SCHEMA:FOPERAND@ IR-ID:TYPE-LOCAL f IR-ID:TYPE-LOCAL =
   qv rv key fl 1 IR-SCHEMA:FOPERAND@ IR-ID:TYPE-LOCAL f IR-ID:TYPE-LOCAL =
   qv rv key fl 0 IR-SCHEMA:FRESULT@ IR-ID:TYPE-LOCAL t IR-ID:TYPE-LOCAL =
   qv rv key fg 0 IR-SCHEMA:FRESULT@ IR-ID:TYPE-LOCAL t IR-ID:TYPE-LOCAL =
   qv rv key fe 0 IR-SCHEMA:FRESULT@ IR-ID:TYPE-LOCAL t IR-ID:TYPE-LOCAL =
   qv rv key fz 0 IR-SCHEMA:FOPERAND@ IR-ID:TYPE-LOCAL f IR-ID:TYPE-LOCAL =
   qv rv key ez 0 IR-SCHEMA:FRESULT@ IR-ID:TYPE-LOCAL t IR-ID:TYPE-LOCAL = ;

: FCMP-SHAPE-CASE ( -- )
   s" a float comparison takes doubles and answers a CELL, which is what a flag is" T-LABEL
   BND [: FCMP-SHAPE-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE
   1 T= 1 T= 2 T= ;

\ Float comparisons are total and need the floating unit: a comparison against a
\ NaN answers false and raises nothing. A trapping schema would oblige the
\ machine stage to reproduce a trap the hardware does not take.
: FCMP-TRAP-BODY ( IR-CTX:ctx -- bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b HIR-OPCODE:FLT HIR:OPCODE {: fl:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:FLTZ HIR:OPCODE {: fz:IR-ID:ir-symbol-id :}
   c b IR-BUILD:FREEZE IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   rv fl IR-SCHEMA:FTRAPS?
   rv fz IR-SCHEMA:FTRAPS?
   rv fl IR-SCHEMA:FFEATURES@ CTARGET:F-SCALAR-FP CTARGET:HAS?
   rv fz IR-SCHEMA:FFEATURES@ CTARGET:F-SCALAR-FP CTARGET:HAS? ;

: FCMP-TRAP-CASE ( -- )
   s" a float comparison is total and needs the floating unit" T-LABEL
   BND [: FCMP-TRAP-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TFALSE TFALSE ;

\ If the double and cell types interned to one identity, every FSHAPE-BODY
\ assertion would still pass.
: FTYPE-BODY ( IR-CTX:ctx -- bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b IR--TYPE-WIDTH:W64 IR--TYPE-SIGN:SIGNED IR-BUILD:INTERN-INT
   {: t:IR-ID:ir-type-id :}
   c b HIR:REAL-TYPE {: f:IR-ID:ir-type-id :}
   t IR-ID:TYPE-LOCAL f IR-ID:TYPE-LOCAL = ;

: FTYPE-CASE ( -- )
   s" the double type and the cell type are two identities" T-LABEL
   BND [: FTYPE-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE ;

\ Float division traps under neither policy, unlike `hir.div`: x/0 answers an
\ infinity and 0/0 the default NaN. A schema copying hir.div's flag would oblige
\ the machine stage to reproduce a trap the hardware does not raise.
: FDIV-TRAP-BODY ( IR-CTX:ctx -- bool bool bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b HIR-OPCODE:FDIV HIR:OPCODE {: fv:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:FSQRT HIR:OPCODE {: fq:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:FADD HIR:OPCODE {: fa:IR-ID:ir-symbol-id :}
   c b IR-BUILD:FREEZE IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   rv fv IR-SCHEMA:FTRAPS?
   rv fq IR-SCHEMA:FTRAPS?
   rv fa IR-SCHEMA:FTRAPS? ;

: FDIV-TRAP-CASES ( -- )
   s" float division is total under a trapping policy, unlike integer division" T-LABEL
   BND [: FDIV-TRAP-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TFALSE TFALSE
   s" and under a wrapping one too" T-LABEL
   WBND [: FDIV-TRAP-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TFALSE TFALSE ;

\ ---- registration refusals ---------------------------------------------------
: PTX-BODY ( IR-CTX:ctx -- )
   DIALECT-NEW drop ;

: SCALAR-PTX-BND ( -- CBIND:binding )
   CTARGET-ARCH:PTX CTARGET-ABI:PTX-KERNEL CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-SCALAR-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

: PTX-REG ( -- )
   PBND [: PTX-BODY ;] IR-CTX:WITH-CONTEXT ;

: NOFP-REG ( -- )
   NOFP-BND [: PTX-BODY ;] IR-CTX:WITH-CONTEXT ;

: TWICE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b HIR:REGISTER ;

: TWICE ( -- )
   BND [: TWICE-BODY ;] IR-CTX:WITH-CONTEXT ;

: REG-REFUSE-CASES ( -- )
   s" shared HIR registers against a coherent GPU target" T-LABEL
   PTX-REG
   SCALAR-PTX-BND [: PTX-BODY ;] IR-CTX:WITH-CONTEXT
   s" and on a machine with no floating unit the double type cannot be interned" T-LABEL
   [: NOFP-REG ;] E-IR-TYPE-TARGET TTHROWSQ
   s" registering the dialect twice into one module is refused" T-LABEL
   [: TWICE ;] E-IR-SCHEMA-DUP TTHROWSQ ;


\ ---- the word model: the modeled vocabulary ----------------------------------
: WORDS-NEW ( IR-CTX:ctx IR-BUILD:builder n n -- IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx b:IR-BUILD:builder rcap:n pcap:n :}
   c b IR-BUILD:MODULE-KEY rcap pcap HIR-WORD:NEW ;

: MODEL-NEW ( IR-CTX:ctx -- IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b HIR-WORD:WORDS HIR-WORD:PICK-CELLS WORDS-NEW
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   c b p r HIR-WORD:REGISTER-WORDS
   b p r ;

\ A capacity refusal must leave the preceding declarations readable through
\ the public model API. It does not depend on the context's allocation layout.
: TORN-REG ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena -- IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   c b p r
   c b p r HIR-WORD:REGISTER-WORDS ;

: TORN-BODY ( IR-CTX:ctx -- n n bool bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b 4 HIR-WORD:PICK-CELLS WORDS-NEW
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   \ `catch` restores both stacks' depth, not their contents: read the locals
   \ bound before the catch.
   c b p r [: TORN-REG ;] catch {: rc:n :}
   2drop 2drop
   rc
   r HIR-WORD:MODELED
   r c b s" +" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@
      HIR-OPCODE:ADD HIR-OPCODE:EQ
   r c b s" /" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@
      HIR-OPCODE:DIV HIR-OPCODE:EQ ;

: TORN-CASE ( -- )
   s" a full word table preserves its first and last accepted declarations" T-LABEL
   BND [: TORN-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE 4 T= E-HIR-CAP T= ;

\ `f-` binds to hir.fsub, not hir.sub; `s>f` and `f>s` bind to different
\ crossings.
: FLOAT-OPS-BODY ( IR-CTX:ctx -- bool bool bool bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c MODEL-NEW {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   r c b s" f+" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ HIR-OPCODE:FADD HIR-OPCODE:EQ
   r c b s" f-" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ HIR-OPCODE:FSUB HIR-OPCODE:EQ
   r c b s" f*" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ HIR-OPCODE:FMUL HIR-OPCODE:EQ
   r c b s" f/" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ HIR-OPCODE:FDIV HIR-OPCODE:EQ
   r c b s" fnegate" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ HIR-OPCODE:FNEG HIR-OPCODE:EQ
   r c b s" fabs" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ HIR-OPCODE:FABS HIR-OPCODE:EQ
   r c b s" fsqrt" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ HIR-OPCODE:FSQRT HIR-OPCODE:EQ
   r c b s" s>f" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ HIR-OPCODE:INTREAL HIR-OPCODE:EQ
   r c b s" f>s" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ HIR-OPCODE:REALINT HIR-OPCODE:EQ ;

: FLOAT-OPS-CASE ( -- )
   s" the nine float words bind to their own operations" T-LABEL
   BND [: FLOAT-OPS-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE ;

\ `f>` binds to hir.fgt, not to hir.flt with swapped operands; `f0<` binds to
\ hir.fltz, its own one-operand operation. The integer rows beside them catch a
\ float word resolved to its integer namesake.
: FCMP-OPS-BODY ( IR-CTX:ctx -- bool bool bool bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c MODEL-NEW {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   r c b s" f<" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ HIR-OPCODE:FLT HIR-OPCODE:EQ
   r c b s" f>" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ HIR-OPCODE:FGT HIR-OPCODE:EQ
   r c b s" f=" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ HIR-OPCODE:FEQ HIR-OPCODE:EQ
   r c b s" f0<" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ HIR-OPCODE:FLTZ HIR-OPCODE:EQ
   r c b s" f0=" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ HIR-OPCODE:FEQZ HIR-OPCODE:EQ
   r c b s" f0<" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ HIR-OPCODE:FLT HIR-OPCODE:EQ 0=
   r c b s" <" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ HIR-OPCODE:LT HIR-OPCODE:EQ
   r c b s" <" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ HIR-OPCODE:FLT HIR-OPCODE:EQ 0=
   r c b s" =" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ HIR-OPCODE:FEQ HIR-OPCODE:EQ 0= ;

: FCMP-OPS-CASE ( -- )
   s" the five float comparisons bind to their own operations, not to the integer ones" T-LABEL
   BND [: FCMP-OPS-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE ;

\ `f<=`, `f>=` and `f<>` are not words of this system (src/habu/habu1.f
\ EMIT-FP-PRIMS): the lookup refuses them like any undeclared word.
: FCMP-ABSENT-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MODEL-NEW {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   r c b s" f<=" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ drop ;

: FCMP-ABSENT ( -- )
   BND [: FCMP-ABSENT-BODY ;] IR-CTX:WITH-CONTEXT ;

: FCMP-ABSENT-CASE ( -- )
   s" and a float relation the engine has no word for is not in the model" T-LABEL
   [: FCMP-ABSENT ;] E-HIR-UNMODELED TTHROWSQ ;

\ MODELS? is the one reader that answers, rather than refuses, about an
\ undeclared word, which is what a local's name is.
: MODELS-BODY ( IR-CTX:ctx -- bool bool )
   {: c:IR-CTX:ctx :}
   c MODEL-NEW {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   r c b s" dup" IR-BUILD:INTERN-SYMBOL HIR-WORD:MODELS?
   r c b s" a" IR-BUILD:INTERN-SYMBOL HIR-WORD:MODELS? ;

: MODELS-CASE ( -- )
   s" the table answers about a word it never declared without refusing" T-LABEL
   BND [: MODELS-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TTRUE ;

\ ---- the data word these cases declare ---------------------------------------
\ A FIXED row carries what the engine says the named word pushes, so the cases
\ create a real data word in their scope and check the row against the address
\ the interpreter answers for its name: two different engine paths.
: CELL-A! ( -- )
   s" CELL-A" 0 search-wl 0<> if exit then
   s" create CELL-A 1 cells allot" evaluate-closed ;

\ ---- a data word asked for the wrong kind of meaning -------------------------

: MODEL-PLUS ( IR-CTX:ctx -- IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx :}
   CELL-A!
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b HIR-WORD:WORDS 1+ HIR-WORD:PICK-CELLS WORDS-NEW
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   c b p r HIR-WORD:REGISTER-WORDS
   c b r  c b s" CELL-A" IR-BUILD:INTERN-SYMBOL  HIR-WORD:DECLARE-FIXED
   b p r ;

\ Asking a data word which operation it is, when its whole meaning is a value.
: FIXED-CLASS-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MODEL-PLUS {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   r c b s" CELL-A" HIR-WORD:KEY-SPELL HIR-WORD:OPCODE@ drop ;

: FIXED-CLASS ( -- )
   BND [: FIXED-CLASS-BODY ;] IR-CTX:WITH-CONTEXT ;

: FIXED-CLASS-CASE ( -- )
   s" a data word asked which operation it is is refused" T-LABEL
   [: FIXED-CLASS ;] E-HIR-CLASS TTHROWSQ ;

\ ---- a word this definition calls --------------------------------------------
\ A callable row's entry, input count and output count have three readers. The
\ three values below differ, so a reader wired to the wrong cell answers
\ another.
$4000 constant CALLEE-ENTRY          \ an instruction address, four-byte aligned
2 constant CALLEE-IN
3 constant CALLEE-OUT

: MODEL-CALL ( IR-CTX:ctx -- IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b HIR-WORD:WORDS 1+ HIR-WORD:PICK-CELLS WORDS-NEW
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   c b p r HIR-WORD:REGISTER-WORDS
   c b r  c b s" OTHER-W" IR-BUILD:INTERN-SYMBOL
   CALLEE-ENTRY CALLEE-IN CALLEE-OUT HIR-WORD:DECLARE-CALLABLE
   b p r ;

: CALLABLE-BODY ( IR-CTX:ctx -- n n n bool )
   {: c:IR-CTX:ctx :}
   c MODEL-CALL {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   r c b s" OTHER-W" HIR-WORD:KEY-SPELL HIR-WORD:ENTRY@
   r c b s" OTHER-W" HIR-WORD:KEY-SPELL HIR-WORD:CALLEE-IN@
   r c b s" OTHER-W" HIR-WORD:KEY-SPELL HIR-WORD:CALLEE-OUT@
   r c b s" OTHER-W" HIR-WORD:KEY-SPELL HIR-WORD:ADMIT
      HIR-MEANING:CALLABLE HIR-MEANING:EQ ;

: CALLABLE-CASE ( -- )
   s" a word this definition calls reads back as declared" T-LABEL
   BND [: CALLABLE-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE CALLEE-OUT T= CALLEE-IN T= CALLEE-ENTRY T= ;

: NO-BIND-SCAN ( ptr u8 n -- ) 2drop ;
: NO-BIND-TOKEN ( ptr u8 n n n n n -- ) 2drop 2drop 2drop ;
: NO-BIND-DONE ( ptr u8 n n -- ) 2drop drop ;

: TERMINAL-MODEL-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b 4 HIR-WORD:PICK-CELLS WORDS-NEW nip
   {: r:IR-ARENA:arena :}
   CHECKER-OWNER:TAPE-DISARM
   1 [: NO-BIND-SCAN ;] [: NO-BIND-TOKEN ;] [: NO-BIND-DONE ;]
      CHECKER-OWNER:TAPE-INSTALL
   CHECKER-OWNER:TAPE-ARM
   c b s" throw" HIR-WORD:KEY-SPELL {: th:IR-ID:ir-symbol-id :}
   s" HIR-THROW-SCAN ( -- ) 1 throw" CHECKER-OWNER:CHECK-UNJUDGED drop
   -1 CHECKER-OWNER:BIND-REGIME!
   c b r th 2 HIR-WORD:RESOLVE-SITE TTRUE
   r th HIR-WORD:CALLEE-DEAD? TTRUE
   r th HIR-WORD:TERMINAL? TTRUE
   CHECKER-OWNER:TAPE-DISARM
   CHECKER-OWNER:TAPE-ARM
   c b s" die" HIR-WORD:KEY-SPELL {: di:IR-ID:ir-symbol-id :}
   s" HIR-DIE-SCAN ( -- ) 1 die" CHECKER-OWNER:CHECK-UNJUDGED drop
   c b r di 2 HIR-WORD:RESOLVE-SITE TTRUE
   r di HIR-WORD:TERMINAL? TTRUE
   0 CHECKER-OWNER:BIND-REGIME!
   CHECKER-OWNER:TAPE-DISARM
   c b s" STATED-DEAD" HIR-WORD:KEY-SPELL {: sy:IR-ID:ir-symbol-id :}
   c b r sy CALLEE-ENTRY 0 0 NDICT:GLUE-NONE true HIR-WORD:DECLARE-BOUND-CALLABLE
   r sy HIR-WORD:CALLEE-DEAD? TTRUE
   r sy HIR-WORD:TERMINAL? TFALSE
   c b s" STATED-THROW" HIR-WORD:KEY-SPELL {: named:IR-ID:ir-symbol-id :}
   c b r named s" throw" NDICT:CALL-TARGET 1 0 NDICT:GLUE-NONE true
   HIR-WORD:DECLARE-BOUND-CALLABLE
   r named HIR-WORD:TERMINAL? TFALSE ;

: TERMINAL-SCHEMA-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b HIR-OPCODE:TERMINAL HIR:OPCODE {: op:IR-ID:ir-symbol-id :}
   c b IR-BUILD:FREEZE IR-BUILD:FSCHEMA-ROWS {: rows:IR-ARENA:view :}
   rows op IR-SCHEMA:FOPERANDS 2 T=
   rows op IR-SCHEMA:FOPERAND-TAIL? TTRUE
   rows op IR-SCHEMA:FRESULTS 0 T=
   rows op IR-SCHEMA:FRESULT-TAIL? TFALSE
   rows op IR-SCHEMA:FSUCCESSORS 0 T=
   rows op IR-SCHEMA:FTERMINATOR? TTRUE
   rows op IR-SCHEMA:FTRAPS? TTRUE
   rows op IR-SCHEMA:FEFFECT@ IR--SCHEMA-EFFECT:READ-WRITE IR--SCHEMA-EFFECT:EQ TTRUE ;

: TERMINAL-CASE ( -- )
   s" only resolved engine primitives are terminal, not stated contracts" T-LABEL
   BND [: TERMINAL-MODEL-BODY ;] IR-CTX:WITH-CONTEXT
   s" a terminal call orders memory and has no results or successors" T-LABEL
   BND [: TERMINAL-SCHEMA-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- what a callable declaration and a callable row refuse -------------------
\ The four refusals run in one context: a throw that unwound out of a context
\ would strand its arenas, and the shared registry is small. Two are category
\ errors (a row read as the meaning it does not carry, both ways); two are this
\ table's facts about a callable: no code at the null address, no call site
\ publishing minus one value. Instruction alignment is the machine dialect's.
align
1 TYPED-BUFFER BC-CTX IR-CTX:ctx
1 TYPED-BUFFER BC-BLD IR-BUILD:builder
1 TYPED-BUFFER BC-ROWS IR-ARENA:arena
variable BC-ENTRY
variable BC-IN
variable BC-OUT

: BC-C ( -- IR-CTX:ctx )         0 BC-CTX @ ;
: BC-B ( -- IR-BUILD:builder )   0 BC-BLD @ ;
: BC-R ( -- IR-ARENA:arena )     0 BC-ROWS @ ;

: BC-DECLARE ( -- )
   BC-C BC-B BC-R  BC-C BC-B s" BAD-W" IR-BUILD:INTERN-SYMBOL
   BC-ENTRY @ BC-IN @ BC-OUT @ HIR-WORD:DECLARE-CALLABLE ;

: BC-STAGE ( n n n -- )
   {: entry:n in:n out:n :}
   entry BC-ENTRY ! in BC-IN ! out BC-OUT ! ;

\ Asking a callable word which operation it is, when its whole meaning is a
\ routine somewhere else; and asking a data word where its callee starts.
: BC-AS-OPCODE ( -- )
   BC-R  BC-C BC-B s" OTHER-W" HIR-WORD:KEY-SPELL  HIR-WORD:OPCODE@ drop ;

: BC-AS-ENTRY ( -- )
   BC-R  BC-C BC-B s" CELL-A" HIR-WORD:KEY-SPELL  HIR-WORD:ENTRY@ drop ;

: MODEL-BOTH ( IR-CTX:ctx -- IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b HIR-WORD:WORDS 2 + HIR-WORD:PICK-CELLS WORDS-NEW
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   CELL-A!
   c b p r HIR-WORD:REGISTER-WORDS
   c b r  c b s" OTHER-W" IR-BUILD:INTERN-SYMBOL
   CALLEE-ENTRY CALLEE-IN CALLEE-OUT HIR-WORD:DECLARE-CALLABLE
   c b r  c b s" CELL-A" IR-BUILD:INTERN-SYMBOL  HIR-WORD:DECLARE-FIXED
   b p r ;

: CALLABLE-REFUSE-BODY ( IR-CTX:ctx -- n n n n )
   {: c:IR-CTX:ctx :}
   c 0 BC-CTX !
   c MODEL-BOTH {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   b 0 BC-BLD !
   r 0 BC-ROWS !
   [: BC-AS-OPCODE ;] catch
   [: BC-AS-ENTRY ;] catch
   0 1 1 BC-STAGE
   [: BC-DECLARE ;] catch
   CALLEE-ENTRY 1 -1 BC-STAGE
   [: BC-DECLARE ;] catch ;

: CALLABLE-REFUSAL-CASES ( -- )
   s" a callable row read as another meaning, and a bad callee, are refused"
   T-LABEL
   BND [: CALLABLE-REFUSE-BODY ;] IR-CTX:WITH-CONTEXT
   E-HIR-CALLEE T= E-HIR-CALLEE T= E-HIR-CLASS T= E-HIR-CLASS T= ;

\ ---- a word model without a module builder -----------------------------------
\ The refusals below use a light model with no IR-BUILD module: a throwing
\ fixture holds its context until the harness exits, and a module of this
\ dialect owns fifteen arenas against a sixty-four slot registry.
: LIGHT ( IR-CTX:ctx n n -- IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key )
   {: c:IR-CTX:ctx rcap:n pcap:n :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key 8 96 IR-SYM:NEW {: sp:IR-ARENA:arena sy:IR-ARENA:arena :}
   c key rcap pcap HIR-WORD:NEW {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   sp sy p r key ;

\ `*` means multiply, `dup` is a rename that puts its one value back twice, and
\ `/` is a named boundary waiting on integer division.
: FILL ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- )
   {: c:IR-CTX:ctx sp:IR-ARENA:arena sy:IR-ARENA:arena
      p:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key :}
   c r sy  c sp sy key s" *" IR-SYM:INTERN  HIR-OPCODE:MUL HIR-WORD:DECLARE-OP
   1 HIR-WORD:BEGIN-RENAME
   0 HIR-WORD:ADD-PICK
   0 HIR-WORD:ADD-PICK
   c p r sy  c sp sy key s" dup" IR-SYM:INTERN  HIR-WORD:DECLARE-RENAME
   c r sy
      c sp sy key s" /" IR-SYM:INTERN
      c sp sy key s" habu-model-integer-division" IR-SYM:INTERN
   HIR-WORD:DECLARE-UNMODELED ;

: MODEL ( IR-CTX:ctx -- IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key )
   {: c:IR-CTX:ctx :}
   c 8 8 LIGHT
   {: sp:IR-ARENA:arena sy:IR-ARENA:arena p:IR-ARENA:arena r:IR-ARENA:arena
      key:IR-ID:ir-module-key :}
   c sp sy p r key FILL
   sp sy p r key ;

\ AT preserves this caller's declaration order, independently of the order in
\ which the built-in vocabulary happens to register its words.
: AT-BODY ( IR-CTX:ctx -- bool bool bool )
   {: c:IR-CTX:ctx :}
   c MODEL
   {: sp:IR-ARENA:arena sy:IR-ARENA:arena p:IR-ARENA:arena r:IR-ARENA:arena
      key:IR-ID:ir-module-key :}
   r key 0 HIR-WORD:AT IR-ID:SYMBOL-LOCAL
      c sp sy key s" *" IR-SYM:INTERN IR-ID:SYMBOL-LOCAL =
   r key 1 HIR-WORD:AT IR-ID:SYMBOL-LOCAL
      c sp sy key s" dup" IR-SYM:INTERN IR-ID:SYMBOL-LOCAL =
   r key 2 HIR-WORD:AT IR-ID:SYMBOL-LOCAL
      c sp sy key s" /" IR-SYM:INTERN IR-ID:SYMBOL-LOCAL = ;

: AT-CASE ( -- )
   s" declared words walk in declaration order" T-LABEL
   BND [: AT-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE ;

\ ---- named boundaries and refusals -------------------------------------------
: REASON-BODY ( IR-CTX:ctx -- n bool bool )
   {: c:IR-CTX:ctx :}
   c MODEL
   {: sp:IR-ARENA:arena sy:IR-ARENA:arena p:IR-ARENA:arena r:IR-ARENA:arena
      key:IR-ID:ir-module-key :}
   c sp sy key s" /" IR-SYM:INTERN {: w:IR-ID:ir-symbol-id :}
   c sp sy key s" habu-model-integer-division" IR-SYM:INTERN {: why:IR-ID:ir-symbol-id :}
   r HIR-WORD:MODELED
   r w HIR-WORD:MEANING@ HIR-MEANING:UNMODELED HIR-MEANING:EQ
   r key w HIR-WORD:REASON@ IR-ID:SYMBOL-LOCAL why IR-ID:SYMBOL-LOCAL = ;

: REASON-CASE ( -- )
   s" a boundary reads back unmodeled and names its capability" T-LABEL
   BND [: REASON-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE 3 T= ;

: UNMOD-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MODEL
   {: sp:IR-ARENA:arena sy:IR-ARENA:arena p:IR-ARENA:arena r:IR-ARENA:arena
      key:IR-ID:ir-module-key :}
   r c sp sy key s" /" IR-SYM:INTERN HIR-WORD:ADMIT drop ;

: UNMOD ( -- )
   BND [: UNMOD-BODY ;] IR-CTX:WITH-CONTEXT ;

\ `xor` stays unmodeled: the subset's five opcodes hold no bitwise operation,
\ and modeling it needs an opcode, an elaboration and a lowering. Every fixture
\ here that needs an arbitrary light-model symbol uses it.
: UNDEC-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MODEL
   {: sp:IR-ARENA:arena sy:IR-ARENA:arena p:IR-ARENA:arena r:IR-ARENA:arena
      key:IR-ID:ir-module-key :}
   r c sp sy key s" xor" IR-SYM:INTERN HIR-WORD:ADMIT drop ;

: UNDEC ( -- )
   BND [: UNDEC-BODY ;] IR-CTX:WITH-CONTEXT ;

: REASON-OF-OP-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MODEL
   {: sp:IR-ARENA:arena sy:IR-ARENA:arena p:IR-ARENA:arena r:IR-ARENA:arena
      key:IR-ID:ir-module-key :}
   r key c sp sy key s" *" IR-SYM:INTERN HIR-WORD:REASON@ drop ;

: REASON-OF-OP ( -- )
   BND [: REASON-OF-OP-BODY ;] IR-CTX:WITH-CONTEXT ;

: OPCODE-OF-RENAME-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MODEL
   {: sp:IR-ARENA:arena sy:IR-ARENA:arena p:IR-ARENA:arena r:IR-ARENA:arena
      key:IR-ID:ir-module-key :}
   r c sp sy key s" dup" IR-SYM:INTERN HIR-WORD:OPCODE@ drop ;

: OPCODE-OF-RENAME ( -- )
   BND [: OPCODE-OF-RENAME-BODY ;] IR-CTX:WITH-CONTEXT ;

: PICKS-OF-OP-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MODEL
   {: sp:IR-ARENA:arena sy:IR-ARENA:arena p:IR-ARENA:arena r:IR-ARENA:arena
      key:IR-ID:ir-module-key :}
   r c sp sy key s" *" IR-SYM:INTERN HIR-WORD:PICKS drop ;

: PICKS-OF-OP ( -- )
   BND [: PICKS-OF-OP-BODY ;] IR-CTX:WITH-CONTEXT ;

: INPUTS-OF-OP-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MODEL
   {: sp:IR-ARENA:arena sy:IR-ARENA:arena p:IR-ARENA:arena r:IR-ARENA:arena
      key:IR-ID:ir-module-key :}
   r c sp sy key s" *" IR-SYM:INTERN HIR-WORD:INPUTS@ drop ;

: INPUTS-OF-OP ( -- )
   BND [: INPUTS-OF-OP-BODY ;] IR-CTX:WITH-CONTEXT ;

: DUP-DECL-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MODEL
   {: sp:IR-ARENA:arena sy:IR-ARENA:arena p:IR-ARENA:arena r:IR-ARENA:arena
      key:IR-ID:ir-module-key :}
   c r sy c sp sy key s" *" IR-SYM:INTERN HIR-OPCODE:ADD HIR-WORD:DECLARE-OP ;

: DUP-DECL ( -- )
   BND [: DUP-DECL-BODY ;] IR-CTX:WITH-CONTEXT ;

: REFUSE-CASES ( -- )
   s" a declared boundary is refused by the gate" T-LABEL
   [: UNMOD ;] E-HIR-UNMODELED TTHROWSQ
   s" a word the model never declared is refused the same way" T-LABEL
   [: UNDEC ;] E-HIR-UNMODELED TTHROWSQ
   s" asking a modeled word for a capability is refused" T-LABEL
   [: REASON-OF-OP ;] E-HIR-CLASS TTHROWSQ
   s" asking a rename for an opcode is refused" T-LABEL
   [: OPCODE-OF-RENAME ;] E-HIR-CLASS TTHROWSQ
   s" asking an operation word for its picks is refused" T-LABEL
   [: PICKS-OF-OP ;] E-HIR-CLASS TTHROWSQ
   s" asking an operation word for its inputs is refused" T-LABEL
   [: INPUTS-OF-OP ;] E-HIR-CLASS TTHROWSQ
   s" declaring one word twice is refused" T-LABEL
   [: DUP-DECL ;] E-HIR-DUP TTHROWSQ ;

\ ---- ownership ---------------------------------------------------------------
\ A second module of the same context. Its symbols and its key belong to it, and
\ neither may enter another module's table.
: FOREIGN-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MODEL
   {: sp:IR-ARENA:arena sy:IR-ARENA:arena p:IR-ARENA:arena r:IR-ARENA:arena
      key:IR-ID:ir-module-key :}
   c 4 4 LIGHT
   {: sp2:IR-ARENA:arena sy2:IR-ARENA:arena p2:IR-ARENA:arena r2:IR-ARENA:arena
      key2:IR-ID:ir-module-key :}
   c r sy2 c sp2 sy2 key2 s" xor" IR-SYM:INTERN HIR-OPCODE:ADD HIR-WORD:DECLARE-OP ;

: FOREIGN ( -- )
   BND [: FOREIGN-BODY ;] IR-CTX:WITH-CONTEXT ;

: FOREIGN-KEY-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MODEL
   {: sp:IR-ARENA:arena sy:IR-ARENA:arena p:IR-ARENA:arena r:IR-ARENA:arena
      key:IR-ID:ir-module-key :}
   c 4 4 LIGHT
   {: sp2:IR-ARENA:arena sy2:IR-ARENA:arena p2:IR-ARENA:arena r2:IR-ARENA:arena
      key2:IR-ID:ir-module-key :}
   r key2 0 HIR-WORD:AT drop ;

: FOREIGN-KEY ( -- )
   BND [: FOREIGN-KEY-BODY ;] IR-CTX:WITH-CONTEXT ;

: OWNER-CASES ( -- )
   s" a symbol of another module cannot be declared" T-LABEL
   [: FOREIGN ;] E-HIR-OWNER TTHROWSQ
   s" another module's key cannot walk this table" T-LABEL
   [: FOREIGN-KEY ;] E-HIR-OWNER TTHROWSQ ;

\ ---- capacity ----------------------------------------------------------------
: CAPZERO-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 4 LIGHT drop drop drop drop drop ;

: CAPZERO ( -- )
   BND [: CAPZERO-BODY ;] IR-CTX:WITH-CONTEXT ;

: ROWFULL-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 3 8 LIGHT
   {: sp:IR-ARENA:arena sy:IR-ARENA:arena p:IR-ARENA:arena r:IR-ARENA:arena
      key:IR-ID:ir-module-key :}
   c sp sy p r key FILL
   c r sy c sp sy key s" xor" IR-SYM:INTERN HIR-OPCODE:ADD HIR-WORD:DECLARE-OP ;

: ROWFULL ( -- )
   BND [: ROWFULL-BODY ;] IR-CTX:WITH-CONTEXT ;

: POOLFULL-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 1 LIGHT
   {: sp:IR-ARENA:arena sy:IR-ARENA:arena p:IR-ARENA:arena r:IR-ARENA:arena
      key:IR-ID:ir-module-key :}
   2 HIR-WORD:BEGIN-RENAME
   1 HIR-WORD:ADD-PICK
   0 HIR-WORD:ADD-PICK
   c p r sy c sp sy key s" swap" IR-SYM:INTERN HIR-WORD:DECLARE-RENAME ;

: POOLFULL ( -- )
   BND [: POOLFULL-BODY ;] IR-CTX:WITH-CONTEXT ;

: CAP-CASES ( -- )
   s" a zero row capacity is refused at creation" T-LABEL
   [: CAPZERO ;] E-HIR-CAP TTHROWSQ
   s" a declaration past the committed row ceiling is refused" T-LABEL
   [: ROWFULL ;] E-HIR-CAP TTHROWSQ
   s" a rename whose picks pass the pool ceiling is refused" T-LABEL
   [: POOLFULL ;] E-HIR-CAP TTHROWSQ ;

\ ---- the rename stage --------------------------------------------------------
\ A refusal that happens while a rename is open leaves it open, so every case
\ that ends that way closes the stage before the next one runs.
: STG-CLOSE ( -- )
   HIR-WORD:ABANDON-RENAME ;

: STG-REOPEN ( -- )
   1 HIR-WORD:BEGIN-RENAME
   1 HIR-WORD:BEGIN-RENAME ;

: STG-PICKLESS ( -- )
   HIR-WORD:ABANDON-RENAME ;

: STG-STRAY-PICK ( -- )
   0 HIR-WORD:ADD-PICK ;

: STG-ENDLESS-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MODEL
   {: sp:IR-ARENA:arena sy:IR-ARENA:arena p:IR-ARENA:arena r:IR-ARENA:arena
      key:IR-ID:ir-module-key :}
   c p r sy c sp sy key s" xor" IR-SYM:INTERN HIR-WORD:DECLARE-RENAME ;

: STG-ENDLESS ( -- )
   BND [: STG-ENDLESS-BODY ;] IR-CTX:WITH-CONTEXT ;

: STG-ABANDONED-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MODEL
   {: sp:IR-ARENA:arena sy:IR-ARENA:arena p:IR-ARENA:arena r:IR-ARENA:arena
      key:IR-ID:ir-module-key :}
   1 HIR-WORD:BEGIN-RENAME
   0 HIR-WORD:ADD-PICK
   HIR-WORD:ABANDON-RENAME
   c p r sy c sp sy key s" xor" IR-SYM:INTERN HIR-WORD:DECLARE-RENAME ;

: STG-ABANDONED ( -- )
   BND [: STG-ABANDONED-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A declaration refused for a reason of its own still consumes the stage. This
\ fixture stages two picks and is refused because the word is already modeled;
\ the one after it stages a single pick and must see exactly that one.
: STG-REFUSED-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MODEL
   {: sp:IR-ARENA:arena sy:IR-ARENA:arena p:IR-ARENA:arena r:IR-ARENA:arena
      key:IR-ID:ir-module-key :}
   2 HIR-WORD:BEGIN-RENAME
   0 HIR-WORD:ADD-PICK
   1 HIR-WORD:ADD-PICK
   c p r sy c sp sy key s" dup" IR-SYM:INTERN HIR-WORD:DECLARE-RENAME ;

: STG-REFUSED ( -- )
   BND [: STG-REFUSED-BODY ;] IR-CTX:WITH-CONTEXT ;

: STG-CLEAN-BODY ( IR-CTX:ctx -- n )
   {: c:IR-CTX:ctx :}
   c MODEL
   {: sp:IR-ARENA:arena sy:IR-ARENA:arena p:IR-ARENA:arena r:IR-ARENA:arena
      key:IR-ID:ir-module-key :}
   1 HIR-WORD:BEGIN-RENAME
   0 HIR-WORD:ADD-PICK
   c p r sy c sp sy key s" xor" IR-SYM:INTERN HIR-WORD:DECLARE-RENAME
   r c sp sy key s" xor" IR-SYM:INTERN HIR-WORD:PICKS ;

: STAGE-CASES ( -- )
   s" opening a rename while one is open is refused" T-LABEL
   [: STG-REOPEN ;] E-HIR-STAGE TTHROWSQ
   STG-CLOSE
   s" abandoning a rename that was never opened is refused" T-LABEL
   [: STG-PICKLESS ;] E-HIR-STAGE TTHROWSQ
   s" a pick without an open rename is refused" T-LABEL
   [: STG-STRAY-PICK ;] E-HIR-STAGE TTHROWSQ
   s" declaring a rename that was never opened is refused" T-LABEL
   [: STG-ENDLESS ;] E-HIR-STAGE TTHROWSQ
   s" an abandoned rename cannot then be declared" T-LABEL
   [: STG-ABANDONED ;] E-HIR-STAGE TTHROWSQ
   s" a declaration refused for its own reason still ends the stage" T-LABEL
   [: STG-REFUSED ;] E-HIR-DUP TTHROWSQ
   s" the rename after a refusal sees only its own picks" T-LABEL
   BND [: STG-CLEAN-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= ;

\ ---- pick rules --------------------------------------------------------------
: PICK-WIDE ( -- )
   1 HIR-WORD:BEGIN-RENAME
   1 HIR-WORD:ADD-PICK ;

: PICK-NEG ( -- )
   1 HIR-WORD:BEGIN-RENAME
   -1 HIR-WORD:ADD-PICK ;

: IN-NEG ( -- )
   -1 HIR-WORD:BEGIN-RENAME ;

: IN-WIDE ( -- )
   5 HIR-WORD:BEGIN-RENAME ;

: PICK-MANY ( -- )
   1 HIR-WORD:BEGIN-RENAME
   9 0 ?do 0 HIR-WORD:ADD-PICK loop ;

: PICK-RULE-CASES ( -- )
   s" a pick naming a value the rename never consumed is refused" T-LABEL
   [: PICK-WIDE ;] E-HIR-PICK TTHROWSQ
   STG-CLOSE
   s" a negative pick is refused" T-LABEL
   [: PICK-NEG ;] E-HIR-PICK TTHROWSQ
   STG-CLOSE
   s" a negative input count is refused" T-LABEL
   [: IN-NEG ;] E-HIR-PICK TTHROWSQ
   s" an input count past the ceiling is refused" T-LABEL
   [: IN-WIDE ;] E-HIR-PICK TTHROWSQ
   s" more picks than the ceiling holds is refused" T-LABEL
   [: PICK-MANY ;] E-HIR-PICK TTHROWSQ
   STG-CLOSE ;

\ ---- bounds ------------------------------------------------------------------
: PICK-PAST-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MODEL
   {: sp:IR-ARENA:arena sy:IR-ARENA:arena p:IR-ARENA:arena r:IR-ARENA:arena
      key:IR-ID:ir-module-key :}
   p r c sp sy key s" dup" IR-SYM:INTERN 2 HIR-WORD:PICK@ drop ;

: PICK-PAST ( -- )
   BND [: PICK-PAST-BODY ;] IR-CTX:WITH-CONTEXT ;

: AT-PAST-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MODEL
   {: sp:IR-ARENA:arena sy:IR-ARENA:arena p:IR-ARENA:arena r:IR-ARENA:arena
      key:IR-ID:ir-module-key :}
   r key 3 HIR-WORD:AT drop ;

: AT-PAST ( -- )
   BND [: AT-PAST-BODY ;] IR-CTX:WITH-CONTEXT ;

: AT-NEG-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MODEL
   {: sp:IR-ARENA:arena sy:IR-ARENA:arena p:IR-ARENA:arena r:IR-ARENA:arena
      key:IR-ID:ir-module-key :}
   r key -1 HIR-WORD:AT drop ;

: AT-NEG ( -- )
   BND [: AT-NEG-BODY ;] IR-CTX:WITH-CONTEXT ;

: BOUND-CASES ( -- )
   s" a pick index past the rename's own count is refused" T-LABEL
   [: PICK-PAST ;] E-HIR-BOUND TTHROWSQ
   s" an inventory index past the count is refused" T-LABEL
   [: AT-PAST ;] E-HIR-BOUND TTHROWSQ
   s" a negative inventory index is refused" T-LABEL
   [: AT-NEG ;] E-HIR-BOUND TTHROWSQ ;

\ ---- swapped and foreign arenas ----------------------------------------------
: SWAPPED-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MODEL
   {: sp:IR-ARENA:arena sy:IR-ARENA:arena p:IR-ARENA:arena r:IR-ARENA:arena
      key:IR-ID:ir-module-key :}
   r p c sp sy key s" dup" IR-SYM:INTERN 0 HIR-WORD:PICK@ drop ;

: SWAPPED ( -- )
   BND [: SWAPPED-BODY ;] IR-CTX:WITH-CONTEXT ;

: ALIEN-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MODEL
   {: sp:IR-ARENA:arena sy:IR-ARENA:arena p:IR-ARENA:arena r:IR-ARENA:arena
      key:IR-ID:ir-module-key :}
   c key 4 NTAPE:NEW HIR-WORD:MODELED drop ;

: ALIEN ( -- )
   BND [: ALIEN-BODY ;] IR-CTX:WITH-CONTEXT ;

: RAGGED-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MODEL
   {: sp:IR-ARENA:arena sy:IR-ARENA:arena p:IR-ARENA:arena r:IR-ARENA:arena
      key:IR-ID:ir-module-key :}
   c r 0 IR-ARENA:PUSH drop
   r HIR-WORD:MODELED drop ;

: RAGGED ( -- )
   BND [: RAGGED-BODY ;] IR-CTX:WITH-CONTEXT ;

: ARENA-CASES ( -- )
   s" the pool and the row table cannot be swapped at a call site" T-LABEL
   [: SWAPPED ;] E-HIR-STATE TTHROWSQ
   s" another package's arena is not a word model" T-LABEL
   [: ALIEN ;] E-HIR-STATE TTHROWSQ
   s" a row table with a partial row is refused" T-LABEL
   [: RAGGED ;] E-HIR-STATE TTHROWSQ ;

\ A pool whose cell count happens to be a whole number of rows: only the two
\ header tags being different tells it from a row table, and only the pair check
\ tells one model's pool from another model's.
: SHAPED-POOL ( IR-CTX:ctx -- IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx :}
   c 4 5 LIGHT
   {: sp:IR-ARENA:arena sy:IR-ARENA:arena p:IR-ARENA:arena r:IR-ARENA:arena
      key:IR-ID:ir-module-key :}
   1 HIR-WORD:BEGIN-RENAME
   5 0 ?do 0 HIR-WORD:ADD-PICK loop
   c sp sy key s" 5dup" IR-SYM:INTERN {: w:IR-ID:ir-symbol-id :}
   c p r sy w HIR-WORD:DECLARE-RENAME
   p r w ;

: SHAPED-OK-BODY ( IR-CTX:ctx -- n n )
   SHAPED-POOL {: p:IR-ARENA:arena r:IR-ARENA:arena w:IR-ID:ir-symbol-id :}
   r w HIR-WORD:PICKS
   p r w 4 HIR-WORD:PICK@ ;

: SHAPED-SWAP-BODY ( IR-CTX:ctx -- )
   SHAPED-POOL {: p:IR-ARENA:arena r:IR-ARENA:arena w:IR-ID:ir-symbol-id :}
   r p w 0 HIR-WORD:PICK@ drop ;

: SHAPED-SWAP ( -- )
   BND [: SHAPED-SWAP-BODY ;] IR-CTX:WITH-CONTEXT ;

: CROSSED-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MODEL
   {: sp:IR-ARENA:arena sy:IR-ARENA:arena p:IR-ARENA:arena r:IR-ARENA:arena
      key:IR-ID:ir-module-key :}
   c 8 8 LIGHT
   {: sp2:IR-ARENA:arena sy2:IR-ARENA:arena p2:IR-ARENA:arena r2:IR-ARENA:arena
      key2:IR-ID:ir-module-key :}
   c sp2 sy2 p2 r2 key2 FILL
   p2 r c sp sy key s" dup" IR-SYM:INTERN 0 HIR-WORD:PICK@ drop ;

: CROSSED ( -- )
   BND [: CROSSED-BODY ;] IR-CTX:WITH-CONTEXT ;

: PAIR-CASES ( -- )
   \ positive control: the row-shaped pool reads back the right way round
   s" a pool that is a whole number of rows long still reads" T-LABEL
   BND [: SHAPED-OK-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 5 T=
   s" a row-shaped pool is still not a row table" T-LABEL
   [: SHAPED-SWAP ;] E-HIR-STATE TTHROWSQ
   s" one model's pool cannot serve another model's rows" T-LABEL
   [: CROSSED ;] E-HIR-OWNER TTHROWSQ ;

\ ---- forged rows -------------------------------------------------------------
\ A raw append writes a row of the right shape and the wrong content. The seven
\ cells follow src/compiler/native/hir-word.f's row order: symbol ordinal,
\ meaning, payload, rename input count, pick count, a callable's multi-cell
\ result grouping, and whether the callee returns. A layout change must change
\ this fixture.
: RAW-ROW ( IR-CTX:ctx IR-ARENA:arena n n n n n -- )
   {: c:IR-CTX:ctx r:IR-ARENA:arena so:n mean:n a:n in:n n:n :}
   c r so IR-ARENA:PUSH drop
   c r mean IR-ARENA:PUSH drop
   c r a IR-ARENA:PUSH drop
   c r in IR-ARENA:PUSH drop
   c r n IR-ARENA:PUSH drop
   c r 0 IR-ARENA:PUSH drop
   c r 0 IR-ARENA:PUSH drop ;

: FORGE ( IR-CTX:ctx n n n n -- IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id IR-ID:ir-module-key )
   {: c:IR-CTX:ctx mean:n a:n in:n n:n :}
   c 4 4 LIGHT
   {: sp:IR-ARENA:arena sy:IR-ARENA:arena p:IR-ARENA:arena r:IR-ARENA:arena
      key:IR-ID:ir-module-key :}
   c sp sy key s" xor" IR-SYM:INTERN {: w:IR-ID:ir-symbol-id :}
   c r w IR-ID:SYMBOL-LOCAL mean a in n RAW-ROW
   p r w key ;

: FORGE-OK-BODY ( IR-CTX:ctx -- bool )
   1 1 0 0 FORGE
   {: p:IR-ARENA:arena r:IR-ARENA:arena w:IR-ID:ir-symbol-id key:IR-ID:ir-module-key :}
   r w HIR-WORD:OPCODE@ HIR-OPCODE:ADD HIR-OPCODE:EQ ;

: FORGE-LITERAL-BODY ( IR-CTX:ctx -- )
   0 0 0 0 FORGE
   {: p:IR-ARENA:arena r:IR-ARENA:arena w:IR-ID:ir-symbol-id key:IR-ID:ir-module-key :}
   r w HIR-WORD:MEANING@ drop ;

: FORGE-LITERAL ( -- )
   BND [: FORGE-LITERAL-BODY ;] IR-CTX:WITH-CONTEXT ;

: FORGE-MEAN-BODY ( IR-CTX:ctx -- )
   10 0 0 0 FORGE
   {: p:IR-ARENA:arena r:IR-ARENA:arena w:IR-ID:ir-symbol-id key:IR-ID:ir-module-key :}
   r w HIR-WORD:MEANING@ drop ;

: FORGE-MEAN ( -- )
   BND [: FORGE-MEAN-BODY ;] IR-CTX:WITH-CONTEXT ;

: FORGE-OPCODE-BODY ( IR-CTX:ctx -- )
   1 HIR:OPCODES 0 0 FORGE
   {: p:IR-ARENA:arena r:IR-ARENA:arena w:IR-ID:ir-symbol-id key:IR-ID:ir-module-key :}
   r w HIR-WORD:OPCODE@ drop ;

: FORGE-OPCODE ( -- )
   BND [: FORGE-OPCODE-BODY ;] IR-CTX:WITH-CONTEXT ;

: FORGE-REASON-BODY ( IR-CTX:ctx -- )
   3 0 0 0 FORGE
   {: p:IR-ARENA:arena r:IR-ARENA:arena w:IR-ID:ir-symbol-id key:IR-ID:ir-module-key :}
   r key w HIR-WORD:REASON@ drop ;

: FORGE-REASON ( -- )
   BND [: FORGE-REASON-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A rename row whose pick window runs past the cells the pool actually holds.
: FORGE-WINDOW-BODY ( IR-CTX:ctx -- )
   2 0 2 2 FORGE
   {: p:IR-ARENA:arena r:IR-ARENA:arena w:IR-ID:ir-symbol-id key:IR-ID:ir-module-key :}
   p r w 0 HIR-WORD:PICK@ drop ;

: FORGE-WINDOW ( -- )
   BND [: FORGE-WINDOW-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A rename row whose stored pick names a value it never consumed.
: FORGE-PICK-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 2 0 1 1 FORGE
   {: p:IR-ARENA:arena r:IR-ARENA:arena w:IR-ID:ir-symbol-id key:IR-ID:ir-module-key :}
   c p 3 IR-ARENA:PUSH drop
   p r w 0 HIR-WORD:PICK@ drop ;

: FORGE-PICK ( -- )
   BND [: FORGE-PICK-BODY ;] IR-CTX:WITH-CONTEXT ;

: FORGE-CASES ( -- )
   \ positive control: the same hand-written row with legal content reads back,
   \ so every rejection below fails for its stated reason
   s" a hand-written row with legal content reads back" T-LABEL
   BND [: FORGE-OK-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE
   s" a row claiming to be a literal is refused" T-LABEL
   [: FORGE-LITERAL ;] E-HIR-CLASS TTHROWSQ
   s" a stored meaning outside the vocabulary is refused" T-LABEL
   [: FORGE-MEAN ;] E-HIR-CLASS TTHROWSQ
   s" a stored opcode outside this dialect is refused" T-LABEL
   [: FORGE-OPCODE ;] E-HIR-OPCODE TTHROWSQ
   s" a boundary row that names nothing is refused" T-LABEL
   [: FORGE-REASON ;] E-HIR-STATE TTHROWSQ
   s" a rename whose picks lie outside the pool is refused" T-LABEL
   [: FORGE-WINDOW ;] E-HIR-STATE TTHROWSQ
   s" a stored pick naming a value never consumed is refused" T-LABEL
   [: FORGE-PICK ;] E-HIR-PICK TTHROWSQ ;

\ ---- the tape join -----------------------------------------------------------
\ A sealed tape of `1 dup * xor`, a string and a character literal, and a word
\ model of the same module. Tape and model share one interner, so a spelling on
\ the tape and one in the model are one identity.
: TAPE-BUILD ( IR-CTX:ctx -- IR-ARENA:view IR-ID:ir-module-key IR-ARENA:arena )
   {: c:IR-CTX:ctx :}
   c 8 8 LIGHT
   {: sp:IR-ARENA:arena sy:IR-ARENA:arena wp:IR-ARENA:arena wr:IR-ARENA:arena
      key:IR-ID:ir-module-key :}
   c sp sy wp wr key FILL
   c key 4 IR-SOURCE:NEW {: sr:IR-ARENA:arena :}
   c key 8 NTAPE:NEW {: tp:IR-ARENA:arena :}
   c sr key s" 1 dup * xor" IR-SOURCE:REGISTER {: s0:IR-ID:ir-source-id :}
   c sp sy key s" 1" IR-SYM:INTERN {: t0:IR-ID:ir-symbol-id :}
   c sp sy key s" dup" IR-SYM:INTERN {: t1:IR-ID:ir-symbol-id :}
   c sp sy key s" *" IR-SYM:INTERN {: t2:IR-ID:ir-symbol-id :}
   c sp sy key s" xor" IR-SYM:INTERN {: t3:IR-ID:ir-symbol-id :}
   c tp sr sy
      sr s0 0 1 IR-SOURCE:SPAN t0 NTAPE-MODE:COMPILING 1 NTAPE:INT-TOKEN
      NTAPE:PUSH drop
   c tp sr sy
      sr s0 2 3 IR-SOURCE:SPAN t1 NTAPE-MODE:COMPILING NTAPE:NAME-TOKEN
      NTAPE:PUSH drop
   c tp sr sy
      sr s0 6 1 IR-SOURCE:SPAN t2 NTAPE-MODE:COMPILING NTAPE:NAME-TOKEN
      NTAPE:PUSH drop
   c tp sr sy
      sr s0 8 3 IR-SOURCE:SPAN t3 NTAPE-MODE:COMPILING NTAPE:NAME-TOKEN
      NTAPE:PUSH drop
   c tp sr sy
      sr s0 0 1 IR-SOURCE:SPAN t0 NTAPE-MODE:COMPILING NTAPE:STRING-TOKEN
      NTAPE:PUSH drop
   c tp sr sy
      sr s0 0 1 IR-SOURCE:SPAN t0 NTAPE-MODE:COMPILING 97 NTAPE:CHAR-TOKEN
      NTAPE:PUSH drop
   tp NTAPE:SEAL key wr ;

: TJ-BODY ( IR-CTX:ctx -- bool bool bool )
   TAPE-BUILD {: v:IR-ARENA:view key:IR-ID:ir-module-key wr:IR-ARENA:arena :}
   v wr 0  v key 0 NTAPE:SPELL@  HIR-WORD:ADMIT-TOKEN HIR-MEANING:LITERAL HIR-MEANING:EQ
   v wr 1  v key 1 NTAPE:SPELL@  HIR-WORD:ADMIT-TOKEN HIR-MEANING:RENAME HIR-MEANING:EQ
   v wr 2  v key 2 NTAPE:SPELL@  HIR-WORD:ADMIT-TOKEN HIR-MEANING:OP HIR-MEANING:EQ ;

: TJ-CASE ( -- )
   s" a tape token is read as a literal, a rename, or an operation" T-LABEL
   BND [: TJ-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE ;

: TJ-UNMOD-BODY ( IR-CTX:ctx -- )
   TAPE-BUILD {: v:IR-ARENA:view key:IR-ID:ir-module-key wr:IR-ARENA:arena :}
   v wr 3  v key 3 NTAPE:SPELL@  HIR-WORD:ADMIT-TOKEN drop ;

: TJ-UNMOD ( -- )
   BND [: TJ-UNMOD-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A string literal is a kind of its own rather than a name: its bytes are the
\ literal's body and there is nothing there to resolve.
: TJ-STRING-BODY ( IR-CTX:ctx -- bool )
   TAPE-BUILD {: v:IR-ARENA:view key:IR-ID:ir-module-key wr:IR-ARENA:arena :}
   v wr 4  v key 4 NTAPE:SPELL@  HIR-WORD:ADMIT-TOKEN HIR-MEANING:STRING-LITERAL HIR-MEANING:EQ ;

: TJ-STRING-CASE ( -- )
   s" a string-literal token is read as a string literal" T-LABEL
   BND [: TJ-STRING-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE ;

: TJ-CHAR-BODY ( IR-CTX:ctx -- bool )
   TAPE-BUILD {: v:IR-ARENA:view key:IR-ID:ir-module-key wr:IR-ARENA:arena :}
   v wr 5  v key 5 NTAPE:SPELL@  HIR-WORD:ADMIT-TOKEN
      HIR-MEANING:LITERAL HIR-MEANING:EQ ;

: TJ-REFUSE-CASES ( -- )
   s" a name the model does not model is refused off the tape" T-LABEL
   [: TJ-UNMOD ;] E-HIR-UNMODELED TTHROWSQ
   s" a character literal is admitted as an integer literal" T-LABEL
   BND [: TJ-CHAR-BODY ;] IR-CTX:WITH-CONTEXT TTRUE ;

\ ---- the case a body may write a dialect word in -----------------------------
\ A row is keyed by the fold of its spelling, the identity the engine gives a
\ name (src/habu/habu2.f LKWCMP, src/habu/habu1.f dictionary find). The fold
\ moves `A`..`Z` and nothing else: "set $20 on every byte" turns `[:` into `{:`,
\ a locals opener. A key is the whole spelling folded, not a prefix. The tape's
\ own symbol is not folded (a refusal names the bytes the body wrote; a string
\ literal's body is its symbol), so a raw symbol that is not its own fold finds
\ no row and fails closed like an undeclared word.
: KEY-OF ( IR-CTX:ctx IR-BUILD:builder ptr u8 n -- n )
   HIR-WORD:KEY-SPELL IR-ID:SYMBOL-LOCAL ;

: RAW-OF ( IR-CTX:ctx IR-BUILD:builder ptr u8 n -- n )
   IR-BUILD:INTERN-SYMBOL IR-ID:SYMBOL-LOCAL ;

: FOLD-BODY ( IR-CTX:ctx -- bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c MODEL-NEW {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   c b s" DUP" KEY-OF  c b s" dup" KEY-OF =
   c b s" DuP" KEY-OF  c b s" dup" KEY-OF =
   c b s" [:" KEY-OF   c b s" {:" KEY-OF = 0=
   c b s" DUP" RAW-OF  c b s" dup" RAW-OF = 0=
   c b s" dup" RAW-OF  c b s" dup" KEY-OF = ;

: FOLD-CASE ( -- )
   s" a spelling's key is its fold, over the letters the engine folds and no other byte" T-LABEL
   BND [: FOLD-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE ;

: CASE-BODY ( IR-CTX:ctx -- bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c MODEL-NEW {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   r  c b s" IF" HIR-WORD:KEY-SPELL  HIR-WORD:CTRL@ HIR-CTRL:OPEN-IF HIR-CTRL:EQ
   r  c b s" BeGiN" HIR-WORD:KEY-SPELL  HIR-WORD:CTRL@ HIR-CTRL:OPEN-BEGIN HIR-CTRL:EQ
   r  c b s" RECURSE" HIR-WORD:KEY-SPELL  HIR-WORD:CTRL@ HIR-CTRL:SELF-CALL HIR-CTRL:EQ
   r  c b s" recurse" HIR-WORD:KEY-SPELL  HIR-WORD:CTRL@ HIR-CTRL:SELF-CALL HIR-CTRL:EQ
   r  c b s" 2DUP" HIR-WORD:KEY-SPELL  HIR-WORD:MEANING@ HIR-MEANING:RENAME HIR-MEANING:EQ
   p r  c b s" OVER" HIR-WORD:KEY-SPELL  0 HIR-WORD:PICK@  1 = ;

: CASE-CASE ( -- )
   s" a word of the dialect is the same word in any case, whichever case it was declared in" T-LABEL
   BND [: CASE-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE ;

: MISS-BODY ( IR-CTX:ctx -- bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c MODEL-NEW {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   r  c b s" IFF" HIR-WORD:KEY-SPELL  HIR-WORD:MODELS? 0=
   r  c b s" DUPX" HIR-WORD:KEY-SPELL  HIR-WORD:MODELS? 0=
   r  c b s" ;:" HIR-WORD:KEY-SPELL  HIR-WORD:MODELS? 0=
   r  c b s" DUP" IR-BUILD:INTERN-SYMBOL  HIR-WORD:MODELS? 0= ;

: MISS-CASE ( -- )
   s" a word the dialect never declared is still no word of it, in any case" T-LABEL
   BND [: MISS-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE ;

\ The two halves of a quotation are dialect words: E-NELAB-QUOT reports a shape
\ the dialect has a place for, E-HIR-UNMODELED one it has none for.
: QUOT-MODEL-BODY ( IR-CTX:ctx -- bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c MODEL-NEW {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   r  c b s" [:" IR-BUILD:INTERN-SYMBOL  HIR-WORD:MODELS?
   r  c b s" ;]" IR-BUILD:INTERN-SYMBOL  HIR-WORD:MODELS?
   r  c b s" [:" IR-BUILD:INTERN-SYMBOL  HIR-WORD:CTRL@
      HIR-CTRL:OPEN-QUOT HIR-CTRL:EQ
   r  c b s" ;]" IR-BUILD:INTERN-SYMBOL  HIR-WORD:CTRL@
      HIR-CTRL:CLOSE-QUOT HIR-CTRL:EQ
   r  c b s" finally" IR-BUILD:INTERN-SYMBOL  HIR-WORD:CTRL@
      HIR-CTRL:FINALLY HIR-CTRL:EQ ;

: QUOT-MODEL-CASE ( -- )
   s" quotation delimiters and finally register within the advertised capacity" T-LABEL
   BND [: QUOT-MODEL-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE ;

\ `do` and `?do` are two rows: only the control action tells them apart, and one
\ row would give `do` the zero-trip guard of `?do` or the reverse. The last line
\ asserts the actions differ; without it an enum whose members all compared
\ equal would pass.
: DO-MODEL-BODY ( IR-CTX:ctx -- bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c MODEL-NEW {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   r  c b s" do" HIR-WORD:KEY-SPELL  HIR-WORD:CTRL@
      HIR-CTRL:OPEN-DO HIR-CTRL:EQ
   r  c b s" ?DO" HIR-WORD:KEY-SPELL  HIR-WORD:CTRL@
      HIR-CTRL:OPEN-DO-SKIP HIR-CTRL:EQ
   r  c b s" do" HIR-WORD:KEY-SPELL  HIR-WORD:CTRL@
      HIR-CTRL:OPEN-DO-SKIP HIR-CTRL:EQ 0=
   HIR-CTRL:OPEN-DO HIR-CTRL:OPEN-DO-SKIP HIR-CTRL:EQ 0= ;

: DO-MODEL-CASE ( -- )
   s" do and ?do are two rows naming two control actions" T-LABEL
   BND [: DO-MODEL-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE ;

\ Only the control action tells `again` from `repeat` and `leave` from `exit`:
\ `again` given `repeat`'s action opens an unreachable block after the loop, and
\ `leave` given `exit`'s branches out of the definition. The last two lines
\ assert the four actions are pairwise different.
: AGAIN-LEAVE-MODEL-BODY ( IR-CTX:ctx -- bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c MODEL-NEW {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   r  c b s" again" HIR-WORD:KEY-SPELL  HIR-WORD:CTRL@
      HIR-CTRL:CLOSE-AGAIN HIR-CTRL:EQ
   r  c b s" LEAVE" HIR-WORD:KEY-SPELL  HIR-WORD:CTRL@
      HIR-CTRL:EARLY-LEAVE HIR-CTRL:EQ
   r  c b s" again" HIR-WORD:KEY-SPELL  HIR-WORD:CTRL@
      HIR-CTRL:CLOSE-REPEAT HIR-CTRL:EQ 0=
   r  c b s" leave" HIR-WORD:KEY-SPELL  HIR-WORD:CTRL@
      HIR-CTRL:EARLY-EXIT HIR-CTRL:EQ 0=
   HIR-CTRL:CLOSE-AGAIN HIR-CTRL:CLOSE-REPEAT HIR-CTRL:EQ 0=
   HIR-CTRL:EARLY-LEAVE HIR-CTRL:EARLY-EXIT HIR-CTRL:EQ 0= ;

: AGAIN-LEAVE-MODEL-CASE ( -- )
   s" again and leave are rows of their own, not repeat and exit" T-LABEL
   BND [: AGAIN-LEAVE-MODEL-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE ;

\ Only the control action tells `i` from `j` (neither stages an operation): `j`
\ given `index` reads the innermost loop at the same size and blocks. The last
\ two lines assert the actions differ and neither is the frame-drop action.
: INDEX-MODEL-BODY ( IR-CTX:ctx -- bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c MODEL-NEW {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   r  c b s" i" HIR-WORD:KEY-SPELL  HIR-WORD:CTRL@
      HIR-CTRL:INDEX HIR-CTRL:EQ
   r  c b s" J" HIR-WORD:KEY-SPELL  HIR-WORD:CTRL@
      HIR-CTRL:OUTER-INDEX HIR-CTRL:EQ
   r  c b s" i" HIR-WORD:KEY-SPELL  HIR-WORD:CTRL@
      HIR-CTRL:OUTER-INDEX HIR-CTRL:EQ 0=
   r  c b s" j" HIR-WORD:KEY-SPELL  HIR-WORD:CTRL@
      HIR-CTRL:INDEX HIR-CTRL:EQ 0=
   HIR-CTRL:INDEX HIR-CTRL:OUTER-INDEX HIR-CTRL:EQ 0=
   HIR-CTRL:OUTER-INDEX HIR-CTRL:DROP-LOOP HIR-CTRL:EQ 0= ;

: INDEX-MODEL-CASE ( -- )
   s" i and j are two rows naming two control actions" T-LABEL
   BND [: INDEX-MODEL-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE ;

\ One word is one row whichever case each declaration wrote: a second row for
\ `FOO` would make the row a lookup reaches depend on declaration order.
: DUP-CASE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b HIR-WORD:WORDS 2 + HIR-WORD:PICK-CELLS WORDS-NEW
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   c b p r HIR-WORD:REGISTER-WORDS
   c b r  c b s" Foo" IR-BUILD:INTERN-SYMBOL
   CALLEE-ENTRY CALLEE-IN CALLEE-OUT HIR-WORD:DECLARE-CALLABLE
   c b r  c b s" FOO" IR-BUILD:INTERN-SYMBOL
   CALLEE-ENTRY CALLEE-IN CALLEE-OUT HIR-WORD:DECLARE-CALLABLE ;

: DUP-CASE ( -- )
   BND [: DUP-CASE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ The frozen door states its key rather than computing it (the note at SYM-CK),
\ so a row declared through it under a spelling that is not its own fold is
\ unreachable and fails closed like an undeclared word.
: STATED-KEY-BODY ( IR-CTX:ctx -- bool bool )
   {: c:IR-CTX:ctx :}
   c MODEL
   {: sp:IR-ARENA:arena sy:IR-ARENA:arena p:IR-ARENA:arena r:IR-ARENA:arena
      key:IR-ID:ir-module-key :}
   c r sy  c sp sy key s" XOR" IR-SYM:INTERN  HIR-OPCODE:XOR HIR-WORD:DECLARE-OP
   r  c sp sy key s" XOR" IR-SYM:INTERN  HIR-WORD:MODELS?
   r  c sp sy key s" xor" IR-SYM:INTERN  HIR-WORD:MODELS? 0= ;

: STATED-KEY-CASE ( -- )
   s" a row declared under a spelling that is not its own fold is reachable by no key" T-LABEL
   BND [: STATED-KEY-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE ;

: KEY-CASES ( -- )
   FOLD-CASE
   CASE-CASE
   MISS-CASE
   QUOT-MODEL-CASE
   DO-MODEL-CASE
   AGAIN-LEAVE-MODEL-CASE
   INDEX-MODEL-CASE
   STATED-KEY-CASE
   s" one word declared twice in two cases is one row declared twice" T-LABEL
   [: DUP-CASE ;] E-HIR-DUP TTHROWSQ ;

\ ---- the schema table this dialect may fill ----------------------------------
\ A schema table's header names its dialect and schema version. HIR:NEW-BUILDER
\ writes this dialect's; registering into an IR-BUILD module created with
\ another is refused.
: OTHER-DIALECT-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-DEFAULT
   c s" ptx" HIR:MAJOR HIR:MINOR IR-BUILD:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b HIR:REGISTER ;

: OTHER-DIALECT ( -- )
   BND [: OTHER-DIALECT-BODY ;] IR-CTX:WITH-CONTEXT ;

\ The right dialect at a schema version these definitions were not written for.
: OTHER-VERSION-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-DEFAULT
   c HIR:NAME HIR:MAJOR 1+ HIR:MINOR IR-BUILD:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b HIR:REGISTER ;

: OTHER-VERSION ( -- )
   BND [: OTHER-VERSION-BODY ;] IR-CTX:WITH-CONTEXT ;

: DIALECT-REFUSE-CASES ( -- )
   s" a module of another dialect refuses this operation family" T-LABEL
   [: OTHER-DIALECT ;] E-HIR-DIALECT TTHROWSQ
   s" a module at another schema version refuses it too" T-LABEL
   [: OTHER-VERSION ;] E-HIR-DIALECT TTHROWSQ ;

\ ---- a declared symbol has to exist ------------------------------------------
\ Each identity below has the right owning module and an ordinal the interner
\ never minted: only the interner can refuse it, and every declarer asks it.
: GHOST-SYM ( IR-ARENA:arena IR-ID:ir-module-key -- IR-ID:ir-symbol-id )
   {: sy:IR-ARENA:arena key:IR-ID:ir-module-key :}
   key sy IR-SYM:SYMBOLS IR-ID:PACK-SYMBOL ;

: GHOST-OP-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MODEL
   {: sp:IR-ARENA:arena sy:IR-ARENA:arena p:IR-ARENA:arena r:IR-ARENA:arena
      key:IR-ID:ir-module-key :}
   c r sy  sy key GHOST-SYM  HIR-OPCODE:ADD HIR-WORD:DECLARE-OP ;

: GHOST-OP ( -- )
   BND [: GHOST-OP-BODY ;] IR-CTX:WITH-CONTEXT ;

: GHOST-WHY-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MODEL
   {: sp:IR-ARENA:arena sy:IR-ARENA:arena p:IR-ARENA:arena r:IR-ARENA:arena
      key:IR-ID:ir-module-key :}
   c r sy
      c sp sy key s" xor" IR-SYM:INTERN
      sy key GHOST-SYM
   HIR-WORD:DECLARE-UNMODELED ;

: GHOST-WHY ( -- )
   BND [: GHOST-WHY-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A rename asks the same question, and asks it after the stage is consumed: the
\ clean-stage case that follows sees only its own picks.
: GHOST-RENAME-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MODEL
   {: sp:IR-ARENA:arena sy:IR-ARENA:arena p:IR-ARENA:arena r:IR-ARENA:arena
      key:IR-ID:ir-module-key :}
   2 HIR-WORD:BEGIN-RENAME
   0 HIR-WORD:ADD-PICK
   1 HIR-WORD:ADD-PICK
   c p r sy  sy key GHOST-SYM  HIR-WORD:DECLARE-RENAME ;

: GHOST-RENAME ( -- )
   BND [: GHOST-RENAME-BODY ;] IR-CTX:WITH-CONTEXT ;

\ Another module's interner cannot answer for this one's symbol.
: XSY-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MODEL
   {: sp:IR-ARENA:arena sy:IR-ARENA:arena p:IR-ARENA:arena r:IR-ARENA:arena
      key:IR-ID:ir-module-key :}
   c 4 4 LIGHT
   {: sp2:IR-ARENA:arena sy2:IR-ARENA:arena p2:IR-ARENA:arena r2:IR-ARENA:arena
      key2:IR-ID:ir-module-key :}
   c r sy2  c sp sy key s" xor" IR-SYM:INTERN  HIR-OPCODE:ADD HIR-WORD:DECLARE-OP ;

: XSY ( -- )
   BND [: XSY-BODY ;] IR-CTX:WITH-CONTEXT ;

: INTERNER-CASES ( -- )
   s" declaring an operation for a symbol nobody interned is refused" T-LABEL
   [: GHOST-OP ;] E-IR-SYM-BOUND TTHROWSQ
   s" a boundary whose capability nobody interned is refused" T-LABEL
   [: GHOST-WHY ;] E-IR-SYM-BOUND TTHROWSQ
   s" a rename for a symbol nobody interned is refused" T-LABEL
   [: GHOST-RENAME ;] E-IR-SYM-BOUND TTHROWSQ
   s" the rename after that refusal still sees only its own picks" T-LABEL
   BND [: STG-CLEAN-BODY ;] IR-CTX:WITH-CONTEXT
   1 T=
   s" another module's interner cannot answer for this table" T-LABEL
   [: XSY ;] E-IR-SYM-OWNER TTHROWSQ ;

\ ---- the tape and the word model on one module -------------------------------
\ The source, spellings, schema table, word model and tape all belong to one
\ IR-BUILD module: a token's spelling is an identity of the interner the model
\ declared its rows from.
: JOINED-TAPE ( IR-CTX:ctx IR-BUILD:builder -- IR-ARENA:view )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b IR-BUILD:MODULE-KEY 4 NTAPE:NEW {: tp:IR-ARENA:arena :}
   c b s" 1 dup *" IR-BUILD:ADD-SOURCE {: s0:IR-ID:ir-source-id :}
   c b tp
      b s0 0 1 IR-BUILD:ADD-SPAN
      c b s" 1" IR-BUILD:INTERN-SYMBOL NTAPE-MODE:COMPILING 1 NTAPE:INT-TOKEN
      NTAPE:PUSH-INTO drop
   c b tp
      b s0 2 3 IR-BUILD:ADD-SPAN
      c b s" dup" IR-BUILD:INTERN-SYMBOL NTAPE-MODE:COMPILING NTAPE:NAME-TOKEN
      NTAPE:PUSH-INTO drop
   c b tp
      b s0 6 1 IR-BUILD:ADD-SPAN
      c b s" *" IR-BUILD:INTERN-SYMBOL NTAPE-MODE:COMPILING NTAPE:NAME-TOKEN
      NTAPE:PUSH-INTO drop
   tp NTAPE:SEAL ;

: JOINED-BODY ( IR-CTX:ctx -- bool bool bool bool n )
   {: c:IR-CTX:ctx :}
   c MODEL-NEW {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   b IR-BUILD:MODULE-KEY {: key:IR-ID:ir-module-key :}
   c b JOINED-TAPE {: v:IR-ARENA:view :}
   v r 0  v key 0 NTAPE:SPELL@  HIR-WORD:ADMIT-TOKEN HIR-MEANING:LITERAL HIR-MEANING:EQ
   v r 1  v key 1 NTAPE:SPELL@  HIR-WORD:ADMIT-TOKEN HIR-MEANING:RENAME HIR-MEANING:EQ
   v r 2  v key 2 NTAPE:SPELL@  HIR-WORD:ADMIT-TOKEN HIR-MEANING:OP HIR-MEANING:EQ
   r  v key 2 NTAPE:SPELL@  HIR-WORD:OPCODE@ HIR-OPCODE:MUL HIR-OPCODE:EQ
   p r  v key 1 NTAPE:SPELL@  1 HIR-WORD:PICK@ ;

: JOINED-CASE ( -- )
   s" one module carries the source tape and the word model together" T-LABEL
   BND [: JOINED-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= TTRUE TTRUE TTRUE TTRUE ;

\ ---- the checker keeps the identities and the API sealed ---------------------
: CHECKER-CASES ( -- )
   s" HRPOS ( IR-ARENA:arena -- n ) HIR-WORD:MODELED"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" HROPFORGE ( n -- HIR:opcode )"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" HRMEANFORGE ( n -- HIR:meaning )"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" HRDECL-CTXLESS ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id HIR:opcode -- ) HIR-WORD:DECLARE-OP"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" HRADMIT-RAW ( IR-ARENA:arena n -- HIR:meaning ) HIR-WORD:ADMIT"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" HRREASON-KEYLESS ( IR-ARENA:arena IR-ID:ir-symbol-id -- IR-ID:ir-symbol-id ) HIR-WORD:REASON@"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" HRREG-BUILDERLESS ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena -- ) HIR-WORD:REGISTER-WORDS"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" HRDECL-INTERNERLESS ( IR-CTX:ctx IR-ARENA:arena IR-ID:ir-symbol-id HIR:opcode -- ) HIR-WORD:DECLARE-OP"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" HRREN-INTERNERLESS ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-symbol-id -- ) HIR-WORD:DECLARE-RENAME"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" HRREG-BUILDER-MODULE ( IR-CTX:ctx IR-BUILD:module -- ) HIR:REGISTER"
      CHECK-QUIET-CANDIDATE! 0 T= ;

\ ---- run ---------------------------------------------------------------------
\ A throwing fixture holds its context until the harness exits, so throwing
\ groups stay small and build a light word model, not a whole module.
: GROUP-SHAPE ( IR-CTX:ctx -- )
   drop
   ARITH-CASE
   SHAPE-CASE
   MEM-SHAPE-CASE ;

: GROUP-POLICY ( IR-CTX:ctx -- )
   drop
   TRAP-CASES
   DIV-TRAP-CASES
   FSHAPE-CASE
   FCMP-SHAPE-CASE
   FCMP-TRAP-CASE
   FTYPE-CASE
   FDIV-TRAP-CASES ;

: GROUP-REG-REFUSE ( IR-CTX:ctx -- )
   drop
   REG-REFUSE-CASES ;

: GROUP-MODEL ( IR-CTX:ctx -- )
   drop
   FLOAT-OPS-CASE
   FCMP-OPS-CASE
   FCMP-ABSENT-CASE
   CALLABLE-CASE
   TERMINAL-CASE
   MODELS-CASE
   AT-CASE ;

: GROUP-FIXED-REFUSE ( IR-CTX:ctx -- )
   drop
   FIXED-CLASS-CASE
   CALLABLE-REFUSAL-CASES ;

: GROUP-REFUSE ( IR-CTX:ctx -- )
   drop
   REASON-CASE
   REFUSE-CASES ;

: GROUP-OWNER ( IR-CTX:ctx -- )
   drop
   OWNER-CASES
   CAP-CASES ;

: GROUP-STAGE ( IR-CTX:ctx -- )
   drop
   STAGE-CASES
   PICK-RULE-CASES
   BOUND-CASES ;

: GROUP-FORGE ( IR-CTX:ctx -- )
   drop
   ARENA-CASES
   PAIR-CASES
   FORGE-CASES ;

: GROUP-TAPE ( IR-CTX:ctx -- )
   drop
   TJ-CASE
   TJ-STRING-CASE
   TJ-REFUSE-CASES ;

: GROUP-DIALECT-REFUSE ( IR-CTX:ctx -- )
   drop
   DIALECT-REFUSE-CASES ;

: GROUP-KEY ( IR-CTX:ctx -- )
   drop
   KEY-CASES ;

: GROUP-INTERNER ( IR-CTX:ctx -- )
   drop
   INTERNER-CASES ;

: GROUP-JOINED ( IR-CTX:ctx -- )
   drop
   JOINED-CASE ;

\ Cached bindings remain owned by their live builder, including on a hit.
1 TYPED-BUFFER MEMO-CTX IR-CTX:ctx
1 TYPED-BUFFER MEMO-BLD IR-BUILD:builder
variable MEMO-IDX

: MEMO-BIND ( -- )
   0 MEMO-CTX @ 0 MEMO-BLD @ MEMO-IDX @ HIR:BIND drop ;

: MEMO-OTHER-CONTEXT ( IR-CTX:ctx -- )
   0 MEMO-CTX !
   [: MEMO-BIND ;] E-IR-BUILD-OWNER TTHROWSQ ;

: MEMO-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 MEMO-CTX ! 0 MEMO-IDX !
   c MOD-NEW {: b:IR-BUILD:builder :}
   b 0 MEMO-BLD !
   c b 0 HIR:BIND {: first:IR-ID:ir-symbol-id :}
   c MOD-NEW {: other:IR-BUILD:builder :}
   c other s" padding" IR-BUILD:INTERN-SYMBOL drop
   c other 0 HIR:BIND {: second:IR-ID:ir-symbol-id :}
   first IR-ID:SYMBOL-OWNER second IR-ID:SYMBOL-OWNER
      IR-ID:MODULE-SAME? TFALSE
   c b 0 HIR:BIND IR-ID:SYMBOL-LOCAL first IR-ID:SYMBOL-LOCAL T=
   c b 0 HIR:BIND IR-ID:SYMBOL-LOCAL first IR-ID:SYMBOL-LOCAL T=
   BND [: MEMO-OTHER-CONTEXT ;] IR-CTX:WITH-CONTEXT
   c 0 MEMO-CTX !
   other IR-BUILD:ABORT
   b IR-BUILD:ABORT
   [: MEMO-BIND ;] E-IR-BUILD-ABORTED TTHROWSQ
   -1 MEMO-IDX ! [: MEMO-BIND ;] E-HIR-OPCODE TTHROWSQ
   HIR:OPCODES MEMO-IDX ! [: MEMO-BIND ;] E-HIR-OPCODE TTHROWSQ
   0 MEMO-IDX !
   c MOD-NEW {: fresh:IR-BUILD:builder :}
   fresh 0 MEMO-BLD !
   c fresh 0 HIR:BIND IR-ID:SYMBOL-OWNER
      fresh IR-BUILD:MODULE@ IR-ID:MODULE-SAME? TTRUE
   c fresh IR-BUILD:FREEZE drop
   [: MEMO-BIND ;] E-IR-BUILD-FROZEN TTHROWSQ ;

: MEMO-CASE ( -- )
   BND [: MEMO-BODY ;] IR-CTX:WITH-CONTEXT
   [: MEMO-BIND ;] E-IR-BUILD-STALE TTHROWSQ ;

\ An unused opcode can be bound without registering a schema. The first
\ use adds exactly one row; repeated use reads the same row.
: LAZY-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN IR-BUILD:PLAN-DEFAULT
   c HIR:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b 0 HIR:BIND {: op:IR-ID:ir-symbol-id :}
   b IR-BUILD:SCHEMAS 0 T=
   c b op IR-BUILD:SCHEMA-DEFINED? TFALSE
   c b HIR-OPCODE:CONST HIR:ENSURE-OP IR-ID:SYMBOL-LOCAL
      op IR-ID:SYMBOL-LOCAL T=
   b IR-BUILD:SCHEMAS 1 T=
   c b HIR-OPCODE:CONST HIR:ENSURE-OP drop
   b IR-BUILD:SCHEMAS 1 T=
   HIR:OPCODES 0 ?do
      c b i HIR:NTH HIR:ENSURE-OP {: named:IR-ID:ir-symbol-id :}
      c b named IR-BUILD:SCHEMA-DEFINED? TTRUE
      c b i HIR:BIND IR-ID:SYMBOL-LOCAL named IR-ID:SYMBOL-LOCAL T=
      c b i HIR:BIND IR-ID:SYMBOL-LOCAL named IR-ID:SYMBOL-LOCAL T=
   loop
   b IR-BUILD:SCHEMAS HIR:OPCODES T= ;

: LAZY-FOREIGN-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN IR-BUILD:PLAN-DEFAULT
   c s" other" 0 0 IR-BUILD:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b HIR-OPCODE:CONST HIR:ENSURE-OP drop ;

: LAZY-FOREIGN ( -- )
   BND [: LAZY-FOREIGN-BODY ;] IR-CTX:WITH-CONTEXT ;

: LAZY-CASE ( -- )
   BND [: LAZY-BODY ;] IR-CTX:WITH-CONTEXT
   [: LAZY-FOREIGN ;] E-HIR-DIALECT TTHROWSQ ;

\ ---- the memo a session prototype fills --------------------------------------
\ Two definitions of one session compile into modules cloned from the session's
\ prototype interner. The second must bind every opcode and attribute key
\ without interning: its module symbol count does not move. A spelling missing
\ from the prototype shows in the count though the identity answered is right.
: PROTO-BUILD ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: k:IR-ID:ir-module-key :}
   c k IR-SYM:CAP-MAX IR-SYM:BYTE-MAX IR-SYM:NEW
   {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r k HIR:PROTOTYPE
   IR-BUILD:PLAN-BEGIN IR-BUILD:PLAN-DEFAULT
   c HIR:NEW-BUILDER {: mb:IR-BUILD:builder :}
   c a r k mb HIR-WORD:SESSION-MODEL ;

: BIND-ALL ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   HIR:OPCODES 0 ?do c b i HIR:BIND drop loop
   c b HIR:KEY-VALUE drop
   c b HIR:KEY-ADDR drop
   c b HIR:KEY-ENTRY drop
   c b HIR:KEY-IN drop
   c b HIR:KEY-OUT drop
   c b HIR:KEY-FUN drop ;

: DEF-BODY ( IR-CTX:ctx -- n n n )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN IR-BUILD:PLAN-DEFAULT
   c HIR:NEW-BUILDER {: b:IR-BUILD:builder :}
   HIR:MISSES-CLEAR
   b IR-BUILD:SYMBOLS {: before:n :}
   c b BIND-ALL
   HIR:MISSES  b IR-BUILD:SYMBOLS  before ;

\ Under AOT the compiler already opened a session, with the production
\ prototype, for this file's load: borrow it when present, open a private one
\ otherwise (a second session is refused by name).
: SESSION-MINE ( -- )
   BND IR-CTX:SESSION-OPEN PROTO-BUILD ;

\ IR-CTX:SESSION-CLOSE stands down what the owner holds in the session (this
\ dialect's prototype, the registered vocabulary); no caller clears flags first.
: SESSION-DROP-MINE ( -- )
   IR-CTX:SESSION-CLOSE ;

\ ---- one vocabulary per load, not one per definition -------------------------
\ The session registers the vocabulary once; a definition's table is an overlay
\ holding only what that definition resolves itself. A definition adds no
\ vocabulary row and costs its module no symbol, yet sees every vocabulary word.
: OV-BODY ( IR-CTX:ctx -- n n bool bool )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN IR-BUILD:PLAN-DEFAULT
   c HIR:NEW-BUILDER {: b:IR-BUILD:builder :}
   b IR-BUILD:SYMBOLS {: before:n :}
   c b 8 0 HIR-WORD:NEW-LINKED {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   b IR-BUILD:SYMBOLS before -
   r HIR-WORD:MODELED
   r  c b s" dup" IR-BUILD:INTERN-SYMBOL  HIR-WORD:MODELS?
   r  c b s" xor" IR-BUILD:INTERN-SYMBOL  HIR-WORD:MODELS? ;

: OV-CASE ( -- )
   IR-CTX:SESSION-LIVE? 0= {: own:bool :}
   own if SESSION-MINE then
   HIR-WORD:SESSION-ROWS {: rows0:n :}
   BND [: OV-BODY ;] IR-CTX:WITH-CONTEXT drop drop drop drop
   BND [: OV-BODY ;] IR-CTX:WITH-CONTEXT
   {: cost:n overlay:n d:bool x:bool :}
   HIR-WORD:SESSION-ROWS {: rows1:n :}
   own if SESSION-DROP-MINE then

   s" the session holds the vocabulary once, and a definition adds none" T-LABEL
   rows0 HIR-WORD:WORDS T=
   rows1 rows0 T=
   s" a definition's overlay starts empty and costs its module no symbol" T-LABEL
   overlay 0 T=
   cost 0 T=
   s" and a vocabulary word this definition never wrote still models" T-LABEL
   d TTRUE
   x TTRUE ;

\ ---- only a clone of THE prototype reads a session row -----------------------
\ A table defers to the session vocabulary only when its module's ordinals are
\ the prototype's (IR-BUILD:CLONED-FROM?): a module that interned its own names
\ holds no vocabulary row and models no vocabulary word.
: PLAIN-BODY ( IR-CTX:ctx -- n bool )
   {: c:IR-CTX:ctx :}
   IR-BUILD:PLAN-BEGIN IR-BUILD:PLAN-DEFAULT
   c s" habu" 1 0 IR-BUILD:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b 8 0 HIR-WORD:NEW-LINKED {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   r HIR-WORD:MODELED
   r  c b s" dup" IR-BUILD:INTERN-SYMBOL  HIR-WORD:MODELS? ;

: PLAIN-CASE ( -- )
   IR-CTX:SESSION-LIVE? 0= {: own:bool :}
   own if SESSION-MINE then
   HIR-WORD:SESSION-ROWS {: rows:n :}
   BND [: PLAIN-BODY ;] IR-CTX:WITH-CONTEXT
   {: overlay:n d:bool :}
   own if SESSION-DROP-MINE then

   s" a session stands and holds its vocabulary" T-LABEL
   rows HIR-WORD:WORDS T=
   s" but a module that interned its own names reads no session row" T-LABEL
   overlay 0 T=
   d TFALSE ;

\ ---- a table of its own module asks the binding gate as it writes ------------
\ The fixture is the HIR-BOUND-XOR package at the top of this file.
: OWN-GATE-CASE ( -- )
   s" a table of its own module models a spelling the engine still owns" T-LABEL
   HIR-BOUND-XOR:AND-MODELED? TTRUE
   s" and refuses one the owning package has taken over" T-LABEL
   HIR-BOUND-XOR:XOR-MODELED? TFALSE ;

: SESSION-MEMO-CASE ( -- )
   IR-CTX:SESSION-LIVE? 0= {: own:bool :}
   own if SESSION-MINE then
   BND [: DEF-BODY ;] IR-CTX:WITH-CONTEXT drop drop drop
   BND [: DEF-BODY ;] IR-CTX:WITH-CONTEXT
   {: misses:n after:n before:n :}
   own if SESSION-DROP-MINE then

   s" the second definition of a session binds without interning" T-LABEL
   misses 0 T=
   s" and its module's symbol count is unchanged by binding" T-LABEL
   after before T= ;

public

: RUN ( -- )
   T-RESET
   SESSION-MEMO-CASE
   OV-CASE
   PLAIN-CASE
   OWN-GATE-CASE
   MEMO-CASE
   LAZY-CASE
   OPCODE-ORDINAL-CASE
   BND [: GROUP-SHAPE ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-POLICY ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-REG-REFUSE ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-MODEL ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-FIXED-REFUSE ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-REFUSE ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-OWNER ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-STAGE ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-FORGE ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-TAPE ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-DIALECT-REFUSE ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-KEY ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-INTERNER ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-JOINED ;] IR-CTX:WITH-CONTEXT
   TORN-CASE
   CHECKER-CASES
   T-REPORT ;

;package

HIR-TEST:RUN

\ native-hir.f - checked straight-line HIR dialect tests.
\
\ Proves the section 7.2 contract of src/compiler/native/hir.f and
\ src/compiler/native/hir-word.f: registering the dialect defines exactly five
\ opcodes and every declared field of each one reads back through the frozen
\ schema table; the may-trap flag follows the compilation unit's overflow policy
\ instead of being fixed in the dialect; a target the dialect cannot run on and
\ a second registration are refused; the source-word model binds the three
\ arithmetic words to operations and the six stack words to compile-time
\ renames that produce no operation; a word the model does not model is refused
\ by name and a declared boundary names the capability it is waiting for;
\ reading a row as a meaning it does not have, a forged row, a swapped arena
\ pair, a foreign module's symbol, a full table, a misused rename stage and an
\ out-of-range pick each reject with their own named code; and the model applied
\ to a sealed source tape reads an integer literal as a literal, a name by its
\ spelling, and refuses a token kind the subset does not model.

require lib/test.f
require test/checker-assert.f
require src/compiler/native/hir-word.f

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

\ The same machine WITHOUT a floating-point unit. The integer subset needs
\ nothing more than the base feature set, so it registers; the float subset
\ declares the floating feature and must not.
: NOFP-BND ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:CONTRACT
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
   c HIR:NEW-BUILDER ;

: DIALECT-NEW ( IR-CTX:ctx -- IR-BUILD:builder )
   {: c:IR-CTX:ctx :}
   c MOD-NEW {: b:IR-BUILD:builder :}
   c b HIR:REGISTER
   b ;

\ ---- the dialect: what registration defines ----------------------------------
\ The opcodes, and the count, so "nothing else was defined" is measured rather
\ than assumed. The family outgrew one case's worth of locals when the
\ comparison and bitwise vocabulary landed, so it is asked in two: the
\ eighteen the straight-line subset started with, and the nine that complete
\ the comparison and bitwise words. The COUNT is asserted once, on the first,
\ and it is what says the second did not quietly define something else too.
: COUNT-BODY ( IR-CTX:ctx -- n bool bool bool bool bool bool bool bool bool bool bool bool bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b HIR-OPCODE:CONST HIR:OPCODE {: k:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:ADD HIR:OPCODE {: a:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:SUB HIR:OPCODE {: s:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:MUL HIR:OPCODE {: u:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:DIV HIR:OPCODE {: v:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:LT HIR:OPCODE {: l:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:LE HIR:OPCODE {: e:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:BR HIR:OPCODE {: j:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:BRZ HIR:OPCODE {: z:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:RETURN HIR:OPCODE {: t:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:MEM HIR:OPCODE {: m:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:LOAD HIR:OPCODE {: d:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:STORE HIR:OPCODE {: w:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:BLOAD HIR:OPCODE {: bl:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:BSTORE HIR:OPCODE {: bw:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:EQUAL HIR:OPCODE {: eqs:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:CALL HIR:OPCODE {: cl:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:WORDCALL HIR:OPCODE {: wc:IR-ID:ir-symbol-id :}
   b IR-BUILD:SCHEMAS
   c b IR-BUILD:FREEZE IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   rv k IR-SCHEMA:FDEFINED?
   rv a IR-SCHEMA:FDEFINED?
   rv s IR-SCHEMA:FDEFINED?
   rv u IR-SCHEMA:FDEFINED?
   rv v IR-SCHEMA:FDEFINED?
   rv l IR-SCHEMA:FDEFINED?
   rv e IR-SCHEMA:FDEFINED?
   rv j IR-SCHEMA:FDEFINED?
   rv z IR-SCHEMA:FDEFINED?
   rv t IR-SCHEMA:FDEFINED?
   rv m IR-SCHEMA:FDEFINED?
   rv d IR-SCHEMA:FDEFINED?
   rv w IR-SCHEMA:FDEFINED?
   rv bl IR-SCHEMA:FDEFINED?
   rv bw IR-SCHEMA:FDEFINED?
   rv eqs IR-SCHEMA:FDEFINED?
   rv cl IR-SCHEMA:FDEFINED?
   rv wc IR-SCHEMA:FDEFINED? ;

: COUNT-CASE ( -- )
   s" registration defines the eighteen opcodes the subset started with" T-LABEL
   BND [: COUNT-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE
   TTRUE TTRUE TTRUE TTRUE TTRUE 44 T= ;

\ The nine that complete the comparison and bitwise vocabulary. `invert` is the
\ one unary operation of the subset and is asked for beside the eight binary
\ ones, because a schema with the wrong number of operands is what would let a
\ caller stage it as a two-value word.
: NEWOPS-BODY ( IR-CTX:ctx -- bool bool bool bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b HIR-OPCODE:GT HIR:OPCODE {: g:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:GE HIR:OPCODE {: ge:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:NE HIR:OPCODE {: n:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:AND HIR:OPCODE {: an:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:OR HIR:OPCODE {: o:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:XOR HIR:OPCODE {: x:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:LSHIFT HIR:OPCODE {: ls:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:RSHIFT HIR:OPCODE {: rs:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:INVERT HIR:OPCODE {: iv:IR-ID:ir-symbol-id :}
   c b IR-BUILD:FREEZE IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   rv g IR-SCHEMA:FDEFINED?
   rv ge IR-SCHEMA:FDEFINED?
   rv n IR-SCHEMA:FDEFINED?
   rv an IR-SCHEMA:FDEFINED?
   rv o IR-SCHEMA:FDEFINED?
   rv x IR-SCHEMA:FDEFINED?
   rv ls IR-SCHEMA:FDEFINED?
   rv rs IR-SCHEMA:FDEFINED?
   rv iv IR-SCHEMA:FDEFINED? ;

: NEWOPS-CASE ( -- )
   s" registration defines the comparison and bitwise opcodes too" T-LABEL
   BND [: NEWOPS-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE ;

\ The dialect names its own table: a caller never spells the name or the
\ version.
: NAMED-BODY ( IR-CTX:ctx -- bool n n )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   b IR-BUILD:MODULE-KEY {: key:IR-ID:ir-module-key :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FSYM-POOL {: pv:IR-ARENA:view :}
   m IR-BUILD:FSYM-ROWS {: yv:IR-ARENA:view :}
   m IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   pv yv  rv key IR-SCHEMA:FDIALECT@  s" hir" IR-SYM:FEQ?
   rv IR-SCHEMA:FMAJOR@
   rv IR-SCHEMA:FMINOR@ ;

: NAMED-CASE ( -- )
   s" the schema table carries the dialect's own name and version" T-LABEL
   BND [: NAMED-BODY ;] IR-CTX:WITH-CONTEXT
   2 T= 0 T= TTRUE ;

\ Every field the arithmetic schema declares, read back off the frozen table.
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

\ The constant carries the one attribute key that gives it its content, and the
\ return is a terminator that takes the word's outputs and has no results.
: SHAPE-BODY ( IR-CTX:ctx -- n n n bool n n bool bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   b IR-BUILD:MODULE-KEY {: key:IR-ID:ir-module-key :}
   c b HIR-OPCODE:CONST HIR:OPCODE {: k:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:RETURN HIR:OPCODE {: r:IR-ID:ir-symbol-id :}
   c b HIR:KEY-VALUE {: vk:IR-ID:ir-symbol-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FSCHEMA-POOL {: qv:IR-ARENA:view :}
   m IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   rv k IR-SCHEMA:FOPERANDS
   rv k IR-SCHEMA:FRESULTS
   rv k IR-SCHEMA:FATTRS
   qv rv key k 0 IR-SCHEMA:FATTR@ IR-ID:SYMBOL-LOCAL vk IR-ID:SYMBOL-LOCAL =
   rv r IR-SCHEMA:FOPERANDS
   rv r IR-SCHEMA:FRESULTS
   rv r IR-SCHEMA:FOPERAND-TAIL?
   rv r IR-SCHEMA:FTERMINATOR? ;

: SHAPE-CASE ( -- )
   s" the constant and the return have the shapes section 7.2 gives them" T-LABEL
   BND [: SHAPE-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE 0 T= 1 T= TTRUE 1 T= 1 T= 0 T= ;

\ The three memory schemas, read back off the frozen table. What is asserted is
\ the shape the ordering rests on: the mint takes nothing and answers one order,
\ the load takes an address and an order and answers a cell and an order, and the
\ store takes a value, an address and an order and answers an order. The types
\ are checked position by position, so an operand list in another order is a
\ different type here rather than a different comment; and both accesses declare
\ a memory effect rather than purity, which is what makes the freeze verifier
\ demand the order they carry.
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

\ The spellings themselves, because every reference this dialect stores is a
\ symbol and a renamed opcode would still read back through the same accessor.
: SPELL-BODY ( IR-CTX:ctx -- bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b HIR-OPCODE:CONST HIR:OPCODE {: k:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:ADD HIR:OPCODE {: a:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:SUB HIR:OPCODE {: s:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:MUL HIR:OPCODE {: u:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:RETURN HIR:OPCODE {: t:IR-ID:ir-symbol-id :}
   c b HIR:KEY-VALUE {: vk:IR-ID:ir-symbol-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FSYM-POOL {: pv:IR-ARENA:view :}
   m IR-BUILD:FSYM-ROWS {: yv:IR-ARENA:view :}
   pv yv k s" hir.const" IR-SYM:FEQ?
   pv yv a s" hir.add" IR-SYM:FEQ?
   pv yv s s" hir.sub" IR-SYM:FEQ?
   pv yv u s" hir.mul" IR-SYM:FEQ?
   pv yv t s" hir.return" IR-SYM:FEQ?
   pv yv vk s" hir.value" IR-SYM:FEQ? ;

: SPELL-CASE ( -- )
   s" the five opcodes and the value key are spelled as declared" T-LABEL
   BND [: SPELL-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE ;

\ Design lines 242 and 243: each opcode names its own semantic rule and its own
\ renderer, and the two are different identities.
: RULE-BODY ( IR-CTX:ctx -- bool bool bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   b IR-BUILD:MODULE-KEY {: key:IR-ID:ir-module-key :}
   c b HIR-OPCODE:MUL HIR:OPCODE {: u:IR-ID:ir-symbol-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FSYM-POOL {: pv:IR-ARENA:view :}
   m IR-BUILD:FSYM-ROWS {: yv:IR-ARENA:view :}
   m IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   pv yv  rv key u IR-SCHEMA:FRULE@  s" hir.rule.mul" IR-SYM:FEQ?
   pv yv  rv key u IR-SCHEMA:FRENDERER@  s" hir.render.mul" IR-SYM:FEQ?
   rv key u IR-SCHEMA:FRULE@ IR-ID:SYMBOL-LOCAL
      rv key u IR-SCHEMA:FRENDERER@ IR-ID:SYMBOL-LOCAL <> ;

: RULE-CASE ( -- )
   s" an opcode names its own rule and its own renderer" T-LABEL
   BND [: RULE-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE ;

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
\ Division may trap under EITHER policy, because what it traps on is a zero
\ divisor and not an overflow: the engine's own `/` branches over a `brk` when
\ the divisor is not zero, so a lowering that dropped the check would answer
\ zero where the interpreted word ends the process. The flag says so under both
\ bindings, which is what makes it a fact about division rather than about the
\ policy the unit was registered with.
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
\ The shapes the float subset rests on, read back off the frozen table. What is
\ asserted is the TYPE at each position, because that is what makes a double a
\ second value class rather than a cell with a different opcode over it: `f+`
\ takes two doubles and answers one, `s>f` takes a cell and answers a double,
\ `f>s` takes a double and answers a cell, and the reinterpretation crosses the
\ same way with no rounding in it. A schema that named the cell type at any of
\ these positions would let a double reach a general register with nothing
\ refusing it.
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
   rv fa IR-SCHEMA:FFEATURES@ CTARGET:F-FP CTARGET:HAS? ;

: FSHAPE-CASE ( -- )
   s" the float opcodes carry the double type at every position, and need the unit" T-LABEL
   BND [: FSHAPE-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE
   1 T= 2 T= ;

\ The five float comparisons, read back off the frozen table. Two things are
\ asserted and both are the shape of a comparison rather than a detail of one:
\ the operands are DOUBLES and the result is a CELL - a comparison answers a Habu
\ flag, which is a number, and a schema that answered a double would put the flag
\ in a register file no branch of this machine reads - and the two comparisons
\ against zero take ONE operand, because the instruction they stand for compares
\ against an immediate the form carries. A schema with two operands there would
\ oblige every lowering to materialise a zero the instruction does not use.
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

\ And they are total and need the floating unit, exactly as the arithmetic is: a
\ comparison against a NaN answers false rather than raising, so a schema that
\ declared one trapping would oblige the machine stage to reproduce a trap the
\ hardware does not take.
: FCMP-TRAP-BODY ( IR-CTX:ctx -- bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b HIR-OPCODE:FLT HIR:OPCODE {: fl:IR-ID:ir-symbol-id :}
   c b HIR-OPCODE:FLTZ HIR:OPCODE {: fz:IR-ID:ir-symbol-id :}
   c b IR-BUILD:FREEZE IR-BUILD:FSCHEMA-ROWS {: rv:IR-ARENA:view :}
   rv fl IR-SCHEMA:FTRAPS?
   rv fz IR-SCHEMA:FTRAPS?
   rv fl IR-SCHEMA:FFEATURES@ CTARGET:F-FP CTARGET:HAS?
   rv fz IR-SCHEMA:FFEATURES@ CTARGET:F-FP CTARGET:HAS? ;

: FCMP-TRAP-CASE ( -- )
   s" a float comparison is total and needs the floating unit" T-LABEL
   BND [: FCMP-TRAP-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TFALSE TFALSE ;

\ The double type is NOT the cell type. Everything above rests on that: if the
\ two interned to one identity, every assertion in FSHAPE-BODY would still pass
\ and nothing anywhere would separate a double from an integer.
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

\ Float division does not trap under either policy, which is where this dialect's
\ float rules and its integer rules part company on purpose: dividing by zero
\ answers an infinity and zero by zero answers the default NaN, both measured on
\ this engine in the survey at the head of tools/codegen-compare-corpus3.f. A
\ schema that copied `hir.div`'s flag would oblige the machine stage to
\ reproduce a trap the hardware does not raise.
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
   s" the native dialect refuses to register against a GPU target" T-LABEL
   [: PTX-REG ;] E-IR-SCHEMA-TARGET TTHROWSQ
   s" and on a machine with no floating unit the double type cannot be interned" T-LABEL
   [: NOFP-REG ;] E-IR-TYPE-TARGET TTHROWSQ
   s" registering the dialect twice into one module is refused" T-LABEL
   [: TWICE ;] E-IR-SCHEMA-DUP TTHROWSQ ;


\ ---- the word model: the modeled vocabulary ----------------------------------
\ The vocabulary REGISTER-WORDS declares, read back off a real module.
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

: OPS-BODY ( IR-CTX:ctx -- n bool bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c MODEL-NEW {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   r HIR-WORD:MODELED
   r c b s" +" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ HIR-OPCODE:ADD HIR-OPCODE:EQ
   r c b s" -" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ HIR-OPCODE:SUB HIR-OPCODE:EQ
   r c b s" *" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ HIR-OPCODE:MUL HIR-OPCODE:EQ
   r c b s" /" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ HIR-OPCODE:DIV HIR-OPCODE:EQ
   r c b s" <" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ HIR-OPCODE:LT HIR-OPCODE:EQ
   r c b s" <=" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ HIR-OPCODE:LE HIR-OPCODE:EQ
   r c b s" =" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ HIR-OPCODE:EQUAL HIR-OPCODE:EQ ;

: OPS-CASE ( -- )
   s" the seven operation words bind to their operations" T-LABEL
   BND [: OPS-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE 61 T= ;

\ The nine float words, each read back off a real model. `f-` binds to hir.fsub
\ and not to hir.sub, and `s>f` and `f>s` bind to two different crossings: a row
\ that named one for the other would compile a body that rounds the wrong way.
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

\ The five float comparisons, read back off a real model. Two rows do the work
\ that a careless table would get wrong. `f>` binds to hir.fgt and NOT to hir.flt:
\ the row says which relation the word is, and a model that reached `f>` by
\ turning `f<`'s operands round would answer hir.flt here. And `f0<` binds to
\ hir.fltz and NOT to hir.flt: the comparison against zero is its own operation
\ with its own operand count, and a row that pointed it at the two-operand form
\ would stage an operation with an operand nothing computed.
\
\ The four integer rows beside them are the falsification: `f<` and `<` are two
\ different words with two different opcodes, so a model that resolved a float
\ word to its integer namesake - or that had let one row's spelling shadow the
\ other's - shows up here rather than as a wrong condition four stages later.
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

\ And the vocabulary has no float relation the engine does not have. `f<=`, `f>=`
\ and `f<>` are not words of this system (src/habu/habu1.f EMIT-FP-PRIMS), so a
\ model row for one would be a promise: the lookup refuses them as names it does
\ not know, exactly as it refuses any other undeclared word.
: FCMP-ABSENT-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MODEL-NEW {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   r c b s" f<=" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ drop ;

: FCMP-ABSENT ( -- )
   BND [: FCMP-ABSENT-BODY ;] IR-CTX:WITH-CONTEXT ;

: FCMP-ABSENT-CASE ( -- )
   s" and a float relation the engine has no word for is not in the model" T-LABEL
   [: FCMP-ABSENT ;] E-HIR-UNMODELED TTHROWSQ ;

\ The nine words the comparison and bitwise vocabulary added, each read back off
\ a real model. `>` binds to hir.gt and NOT to hir.lt: the row says which
\ relation the word is, and a model that reached `>` by turning `<`'s operands
\ round would answer hir.lt here.
: OPS2-BODY ( IR-CTX:ctx -- bool bool bool bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c MODEL-NEW {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   r c b s" >" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ HIR-OPCODE:GT HIR-OPCODE:EQ
   r c b s" >=" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ HIR-OPCODE:GE HIR-OPCODE:EQ
   r c b s" <>" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ HIR-OPCODE:NE HIR-OPCODE:EQ
   r c b s" and" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ HIR-OPCODE:AND HIR-OPCODE:EQ
   r c b s" or" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ HIR-OPCODE:OR HIR-OPCODE:EQ
   r c b s" xor" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ HIR-OPCODE:XOR HIR-OPCODE:EQ
   r c b s" lshift" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ HIR-OPCODE:LSHIFT HIR-OPCODE:EQ
   r c b s" rshift" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ HIR-OPCODE:RSHIFT HIR-OPCODE:EQ
   r c b s" invert" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ HIR-OPCODE:INVERT HIR-OPCODE:EQ ;

: OPS2-CASE ( -- )
   s" the comparison and bitwise words bind to their own relations" T-LABEL
   BND [: OPS2-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE ;

\ The three control words the `while`/`repeat` and `else` vocabulary added, read
\ back off a real model beside the two they stand next to. Each one has a
\ neighbour it could plausibly have been declared as, and the row is what tells
\ them apart: `while` is the middle of a loop and NOT the closer `until` is,
\ `repeat` is a second closer for `begin` and NOT a second opener, and `else` is
\ the middle of an `if` and NOT the closer `then` is. A registration that bound
\ any of the three to its neighbour reads back here as that neighbour.
: CTRL-BODY ( IR-CTX:ctx -- bool bool bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c MODEL-NEW {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   r c b s" while" IR-BUILD:INTERN-SYMBOL HIR-WORD:CTRL@
      HIR-CTRL:MID-WHILE HIR-CTRL:EQ
   r c b s" repeat" IR-BUILD:INTERN-SYMBOL HIR-WORD:CTRL@
      HIR-CTRL:CLOSE-REPEAT HIR-CTRL:EQ
   r c b s" else" IR-BUILD:INTERN-SYMBOL HIR-WORD:CTRL@
      HIR-CTRL:MID-ELSE HIR-CTRL:EQ
   r c b s" until" IR-BUILD:INTERN-SYMBOL HIR-WORD:CTRL@
      HIR-CTRL:CLOSE-UNTIL HIR-CTRL:EQ
   r c b s" begin" IR-BUILD:INTERN-SYMBOL HIR-WORD:CTRL@
      HIR-CTRL:OPEN-BEGIN HIR-CTRL:EQ
   r c b s" then" IR-BUILD:INTERN-SYMBOL HIR-WORD:CTRL@
      HIR-CTRL:CLOSE-IF HIR-CTRL:EQ
   r c b s" while" IR-BUILD:INTERN-SYMBOL HIR-WORD:MEANING@
      HIR-MEANING:CONTROL HIR-MEANING:EQ
   r c b s" repeat" IR-BUILD:INTERN-SYMBOL HIR-WORD:MEANING@
      HIR-MEANING:CONTROL HIR-MEANING:EQ ;

: CTRL-CASE ( -- )
   s" while, repeat and else bind to their own control actions" T-LABEL
   BND [: CTRL-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE ;

\ `0=` and `cells` are a literal and an operation, exactly as `1-` is, and the
\ constant each carries is what makes it the word it is: `0=` is an equality
\ against ZERO and `cells` is a multiplication by EIGHT. Both halves of both
\ rows are read back, because a row carrying the right opcode with the wrong
\ constant compiles to a word that computes something else.
: STEP2-BODY ( IR-CTX:ctx -- bool n bool n )
   {: c:IR-CTX:ctx :}
   c MODEL-NEW {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   r c b s" 0=" IR-BUILD:INTERN-SYMBOL HIR-WORD:CONST-OPCODE@
      HIR-OPCODE:EQUAL HIR-OPCODE:EQ
   r c b s" 0=" IR-BUILD:INTERN-SYMBOL HIR-WORD:CONST-VALUE@
   r c b s" cells" IR-BUILD:INTERN-SYMBOL HIR-WORD:CONST-OPCODE@
      HIR-OPCODE:MUL HIR-OPCODE:EQ
   r c b s" cells" IR-BUILD:INTERN-SYMBOL HIR-WORD:CONST-VALUE@ ;

: STEP2-CASE ( -- )
   s" 0= is an equality against zero and cells a multiplication by eight" T-LABEL
   BND [: STEP2-BODY ;] IR-CTX:WITH-CONTEXT
   8 T= TTRUE 0 T= TTRUE ;

\ `2drop` consumes two values and puts neither back, which is the whole of what
\ the row says: a pick count of zero over an input count of two.
: DROP2-BODY ( IR-CTX:ctx -- n n )
   {: c:IR-CTX:ctx :}
   c MODEL-NEW {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   r c b s" 2drop" IR-BUILD:INTERN-SYMBOL HIR-WORD:INPUTS@
   r c b s" 2drop" IR-BUILD:INTERN-SYMBOL HIR-WORD:PICKS ;

: DROP2-CASE ( -- )
   s" 2drop consumes two values and puts neither back" T-LABEL
   BND [: DROP2-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 2 T= ;

\ ---- the two halves of a typed locals group ----------------------------------
\ Neither stages an operation, so what the word model has to say about them is
\ only which half each one is - and a row read as the other half, or as any
\ other meaning, is a category error rather than a wrong answer.
: LOCALS-MEAN-BODY ( IR-CTX:ctx -- bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c MODEL-NEW {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   r c b s" {:" IR-BUILD:INTERN-SYMBOL HIR-WORD:ADMIT
      HIR-MEANING:OPEN-LOCALS HIR-MEANING:EQ
   r c b s" :}" IR-BUILD:INTERN-SYMBOL HIR-WORD:ADMIT
      HIR-MEANING:CLOSE-LOCALS HIR-MEANING:EQ
   r c b s" {:" IR-BUILD:INTERN-SYMBOL HIR-WORD:ADMIT
      HIR-MEANING:CLOSE-LOCALS HIR-MEANING:EQ
   r c b s" {:" IR-BUILD:INTERN-SYMBOL HIR-WORD:MODELS? ;

: LOCALS-MEAN-CASE ( -- )
   s" the two halves of a locals group are declared and each is itself" T-LABEL
   BND [: LOCALS-MEAN-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TFALSE TTRUE TTRUE ;

\ MODELS? is the one reader that answers about a word the table never declared
\ rather than refusing, which is what a name the PROGRAM chose for a local is.
: MODELS-BODY ( IR-CTX:ctx -- bool bool )
   {: c:IR-CTX:ctx :}
   c MODEL-NEW {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   r c b s" dup" IR-BUILD:INTERN-SYMBOL HIR-WORD:MODELS?
   r c b s" a" IR-BUILD:INTERN-SYMBOL HIR-WORD:MODELS? ;

: MODELS-CASE ( -- )
   s" the table answers about a word it never declared without refusing" T-LABEL
   BND [: MODELS-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TTRUE ;

\ Where a typed local's annotation is cut off. `a:n` declares `a`; an
\ unannotated local is its whole spelling; and the separator is the FIRST colon,
\ so a type that carries one of its own keeps the same name.
: ANN-BODY ( -- n n n )
   s" a:n" HIR-WORD:LOCAL-NAME-LEN
   s" a" HIR-WORD:LOCAL-NAME-LEN
   s" ref:IR-CTX:ctx" HIR-WORD:LOCAL-NAME-LEN ;

: ANN-CASE ( -- )
   s" a local's name is what stands before its annotation" T-LABEL
   ANN-BODY
   3 T= 1 T= 1 T= ;

\ ---- the memory words and the data word --------------------------------------
\ The four memory words bind to the four memory operations - one pair per access
\ width, because the width is a form of the dialect and not a field of one form,
\ so `c@` and `@` are different opcodes and not one opcode read two ways - the
\ increment binds to
\ an addition of one - which is what makes `1+` one token of source and two
\ operations rather than an opcode this dialect does not have - and a `create`d
\ data word is declared by the caller with the value it pushes. The last one is
\ how a definition that mentions a data word compiles at all, so what is asserted
\ is the value it reads back as and the meaning the gate answers for it.
4096 constant DATUM

: MODEL-PLUS ( IR-CTX:ctx -- IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b HIR-WORD:WORDS 1+ HIR-WORD:PICK-CELLS WORDS-NEW
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   c b p r HIR-WORD:REGISTER-WORDS
   c b r  c b s" CELL-A" IR-BUILD:INTERN-SYMBOL  DATUM HIR-WORD:DECLARE-FIXED
   b p r ;

: MEMWORD-BODY ( IR-CTX:ctx -- n bool bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c MODEL-PLUS {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   r c b s" CELL-A" IR-BUILD:INTERN-SYMBOL HIR-WORD:FIXED-VALUE@
   r c b s" CELL-A" IR-BUILD:INTERN-SYMBOL HIR-WORD:ADMIT
      HIR-MEANING:FIXED HIR-MEANING:EQ
   r c b s" @" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@
      HIR-OPCODE:LOAD HIR-OPCODE:EQ
   r c b s" !" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@
      HIR-OPCODE:STORE HIR-OPCODE:EQ
   r c b s" c@" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@
      HIR-OPCODE:BLOAD HIR-OPCODE:EQ
   r c b s" c!" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@
      HIR-OPCODE:BSTORE HIR-OPCODE:EQ
   r c b s" c@" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@
      HIR-OPCODE:LOAD HIR-OPCODE:EQ 0=
   r c b s" 1+" IR-BUILD:INTERN-SYMBOL HIR-WORD:CONST-OPCODE@
      HIR-OPCODE:ADD HIR-OPCODE:EQ ;

: MEMWORD-CASE ( -- )
   s" the memory words, the increment and a data word read back as declared" T-LABEL
   BND [: MEMWORD-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE DATUM T= ;

\ Asking a data word which operation it is, when its whole meaning is a value.
: FIXED-CLASS-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MODEL-PLUS {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   r c b s" CELL-A" IR-BUILD:INTERN-SYMBOL HIR-WORD:OPCODE@ drop ;

: FIXED-CLASS ( -- )
   BND [: FIXED-CLASS-BODY ;] IR-CTX:WITH-CONTEXT ;

: FIXED-CLASS-CASE ( -- )
   s" a data word asked which operation it is is refused" T-LABEL
   [: FIXED-CLASS ;] E-HIR-CLASS TTHROWSQ ;

\ ---- a word this definition calls --------------------------------------------
\ A callable row carries three numbers - where the callee's code starts and what
\ its declared effect is - and they are three separate readers because a call
\ site publishes as many values as the callee takes and reads back as many as it
\ leaves, and those two are different questions. The three values below are all
\ different, so a reader wired to the wrong cell answers one of the others.
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
   r c b s" OTHER-W" IR-BUILD:INTERN-SYMBOL HIR-WORD:ENTRY@
   r c b s" OTHER-W" IR-BUILD:INTERN-SYMBOL HIR-WORD:CALLEE-IN@
   r c b s" OTHER-W" IR-BUILD:INTERN-SYMBOL HIR-WORD:CALLEE-OUT@
   r c b s" OTHER-W" IR-BUILD:INTERN-SYMBOL HIR-WORD:ADMIT
      HIR-MEANING:CALLABLE HIR-MEANING:EQ ;

: CALLABLE-CASE ( -- )
   s" a word this definition calls reads back as declared" T-LABEL
   BND [: CALLABLE-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE CALLEE-OUT T= CALLEE-IN T= CALLEE-ENTRY T= ;

\ ---- what a callable declaration and a callable row refuse -------------------
\ Four refusals, all measured inside ONE context. Each of them throws, and a
\ throw that unwound out of a context would strand the arenas that context had
\ built - the registry is small and shared, so a suite that opened a context per
\ refusal would run out and the later cases would fail for THAT rather than for
\ what they are about. So the context is opened once, the four attempts are
\ caught inside it, and what the case asserts is the four codes.
\
\ THE FOUR ARE TWO PAIRS. The first two are category errors - a row read as a
\ meaning it does not carry, in both directions - and the second two are the two
\ facts this table owns about a callable declaration: no code lives at the null
\ address, and a call site cannot publish minus one value. Whether the address is
\ the address of a whole INSTRUCTION is the machine dialect's field and is
\ asserted in test/compiler/native-a64ir.f, so it is deliberately not restated
\ here.
here CELL 1- and CELL swap - CELL 1- and allot
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
   BC-R  BC-C BC-B s" OTHER-W" IR-BUILD:INTERN-SYMBOL  HIR-WORD:OPCODE@ drop ;

: BC-AS-ENTRY ( -- )
   BC-R  BC-C BC-B s" CELL-A" IR-BUILD:INTERN-SYMBOL  HIR-WORD:ENTRY@ drop ;

\ The model both category errors are asked of: the dialect's vocabulary, one
\ callable word and one data word, so each reader can be pointed at a row of the
\ other meaning.
: MODEL-BOTH ( IR-CTX:ctx -- IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx :}
   c DIALECT-NEW {: b:IR-BUILD:builder :}
   c b HIR-WORD:WORDS 2 + HIR-WORD:PICK-CELLS WORDS-NEW
   {: p:IR-ARENA:arena r:IR-ARENA:arena :}
   c b p r HIR-WORD:REGISTER-WORDS
   c b r  c b s" OTHER-W" IR-BUILD:INTERN-SYMBOL
   CALLEE-ENTRY CALLEE-IN CALLEE-OUT HIR-WORD:DECLARE-CALLABLE
   c b r  c b s" CELL-A" IR-BUILD:INTERN-SYMBOL  DATUM HIR-WORD:DECLARE-FIXED
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

\ One rename, folded into three numbers: how many values it consumes, how many
\ it puts back, and the whole pick list in order as decimal digits, each pick
\ plus one, so a single comparison pins the list and its order.
: REN ( IR-CTX:ctx IR-BUILD:builder IR-ARENA:arena IR-ARENA:arena ptr u8 n -- n n n )
   {: c:IR-CTX:ctx b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena
      a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   c b a u IR-BUILD:INTERN-SYMBOL {: id:IR-ID:ir-symbol-id :}
   r id HIR-WORD:INPUTS@
   r id HIR-WORD:PICKS
   0
   r id HIR-WORD:PICKS 0 ?do
      10 * p r id i HIR-WORD:PICK@ 1+ +
   loop ;

: RENAME-BODY ( IR-CTX:ctx -- n n n n n n n n n n n n n n n n n n )
   {: c:IR-CTX:ctx :}
   c MODEL-NEW {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   c b p r s" dup" REN
   c b p r s" drop" REN
   c b p r s" swap" REN
   c b p r s" over" REN
   c b p r s" nip" REN
   c b p r s" rot" REN ;

\ `nip` consumes two and puts back the one that was on top, so its whole list is
\ the single depth zero. `rot` consumes a b c and leaves b c a, and its list read
\ bottom first is the depth of b, the depth of c and the depth of a - 1 0 2,
\ which REN folds to 213. Neither neighbouring rotation folds to that number:
\ `-rot` would be 0 2 1 and so 132, and leaving the values alone would be 2 1 0
\ and so 321.
: RENAME-CASE ( -- )
   s" the six stack words are renames with exactly their picks" T-LABEL
   BND [: RENAME-BODY ;] IR-CTX:WITH-CONTEXT
   213 T= 3 T= 3 T=
   1 T= 1 T= 2 T=
   212 T= 3 T= 2 T=
   12 T= 2 T= 2 T=
   0 T= 0 T= 1 T=
   11 T= 2 T= 1 T= ;

\ A rename is a meaning, not an operation, and an arithmetic word is the other
\ way round.
: MEAN-BODY ( IR-CTX:ctx -- bool bool )
   {: c:IR-CTX:ctx :}
   c MODEL-NEW {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   r c b s" over" IR-BUILD:INTERN-SYMBOL HIR-WORD:ADMIT
      HIR-MEANING:RENAME HIR-MEANING:EQ
   r c b s" +" IR-BUILD:INTERN-SYMBOL HIR-WORD:ADMIT
      HIR-MEANING:OP HIR-MEANING:EQ ;

: MEAN-CASE ( -- )
   s" the gate answers rename for a stack word and op for an arithmetic one" T-LABEL
   BND [: MEAN-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE ;

\ Declaration order is observable, which is what an inventory walks: the four
\ arithmetic words are declared first, then the six comparisons, the six bitwise
\ words, the four step words, the four memory words, the nine float words, the
\ five float comparisons, the thirteen control words and the two halves of a
\ locals group, with the renames at the end of the walk.
: AT-BODY ( IR-CTX:ctx -- bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c MODEL-NEW {: b:IR-BUILD:builder p:IR-ARENA:arena r:IR-ARENA:arena :}
   b IR-BUILD:MODULE-KEY {: key:IR-ID:ir-module-key :}
   r key 0 HIR-WORD:AT IR-ID:SYMBOL-LOCAL
      c b s" +" IR-BUILD:INTERN-SYMBOL IR-ID:SYMBOL-LOCAL =
   r key 9 HIR-WORD:AT IR-ID:SYMBOL-LOCAL
      c b s" <>" IR-BUILD:INTERN-SYMBOL IR-ID:SYMBOL-LOCAL =
   r key 53 HIR-WORD:AT IR-ID:SYMBOL-LOCAL
      c b s" 2dup" IR-BUILD:INTERN-SYMBOL IR-ID:SYMBOL-LOCAL =
   r key 58 HIR-WORD:AT IR-ID:SYMBOL-LOCAL
      c b s" nip" IR-BUILD:INTERN-SYMBOL IR-ID:SYMBOL-LOCAL =
   r key 60 HIR-WORD:AT IR-ID:SYMBOL-LOCAL
      c b s" 2drop" IR-BUILD:INTERN-SYMBOL IR-ID:SYMBOL-LOCAL = ;

: AT-CASE ( -- )
   s" declared words walk in declaration order" T-LABEL
   BND [: AT-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE ;

\ ---- a word model without a module builder -----------------------------------
\ Every refusal below is measured against a light model: a plain module of the
\ context, its own symbol interner, and one word of each meaning. It needs no
\ IR-BUILD module, which matters because a fixture that throws holds its context
\ until the enclosing harness exits, and a module of this dialect owns fifteen
\ arenas against a sixty-four slot registry.
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

\ A word no table here declared. The spelling is `xor` rather than a stack word,
\ because the subset's five opcodes are a closed family and it holds no bitwise
\ operation at all: modeling `xor` would mean adding an opcode, an elaboration
\ and a lowering, which is a much larger commitment than adding a rename. So the
\ spelling stays genuinely unmodeled while the rename vocabulary grows, and this
\ fixture keeps testing what it says it tests. Every other fixture in this file
\ that needs an arbitrary symbol of the light model uses the same spelling.
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
\ A holder who bypasses the package and appends raw cells writes a row whose
\ shape is right and whose content is not. The five cells are the row shape
\ src/compiler/native/hir-word.f commits to, in its order: the word's symbol
\ ordinal, the stored meaning, the payload, the rename's input count, and its
\ pick count. A change to that layout has to change this fixture too.
: RAW-ROW ( IR-CTX:ctx IR-ARENA:arena n n n n n -- )
   {: c:IR-CTX:ctx r:IR-ARENA:arena so:n mean:n a:n in:n n:n :}
   c r so IR-ARENA:PUSH drop
   c r mean IR-ARENA:PUSH drop
   c r a IR-ARENA:PUSH drop
   c r in IR-ARENA:PUSH drop
   c r n IR-ARENA:PUSH drop ;

\ One empty table, one forged row, and the symbol that names it.
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
   1 44 0 0 FORGE
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
\ A sealed source tape of `1 dup * xor`, then a string and a character literal,
\ and a word model of the same module. The tape and the model share one symbol
\ interner, which is what makes a spelling on the tape and a spelling in the
\ model the same identity.
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
   v key wr 0 HIR-WORD:ADMIT-TOKEN HIR-MEANING:LITERAL HIR-MEANING:EQ
   v key wr 1 HIR-WORD:ADMIT-TOKEN HIR-MEANING:RENAME HIR-MEANING:EQ
   v key wr 2 HIR-WORD:ADMIT-TOKEN HIR-MEANING:OP HIR-MEANING:EQ ;

: TJ-CASE ( -- )
   s" a tape token is read as a literal, a rename, or an operation" T-LABEL
   BND [: TJ-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE ;

: TJ-UNMOD-BODY ( IR-CTX:ctx -- )
   TAPE-BUILD {: v:IR-ARENA:view key:IR-ID:ir-module-key wr:IR-ARENA:arena :}
   v key wr 3 HIR-WORD:ADMIT-TOKEN drop ;

: TJ-UNMOD ( -- )
   BND [: TJ-UNMOD-BODY ;] IR-CTX:WITH-CONTEXT ;

: TJ-STRING-BODY ( IR-CTX:ctx -- )
   TAPE-BUILD {: v:IR-ARENA:view key:IR-ID:ir-module-key wr:IR-ARENA:arena :}
   v key wr 4 HIR-WORD:ADMIT-TOKEN drop ;

: TJ-STRING ( -- )
   BND [: TJ-STRING-BODY ;] IR-CTX:WITH-CONTEXT ;

: TJ-CHAR-BODY ( IR-CTX:ctx -- )
   TAPE-BUILD {: v:IR-ARENA:view key:IR-ID:ir-module-key wr:IR-ARENA:arena :}
   v key wr 5 HIR-WORD:ADMIT-TOKEN drop ;

: TJ-CHAR ( -- )
   BND [: TJ-CHAR-BODY ;] IR-CTX:WITH-CONTEXT ;

: TJ-REFUSE-CASES ( -- )
   s" a name the model does not model is refused off the tape" T-LABEL
   [: TJ-UNMOD ;] E-HIR-UNMODELED TTHROWSQ
   s" a string literal is a kind this subset does not model" T-LABEL
   [: TJ-STRING ;] E-HIR-KIND TTHROWSQ
   s" a character literal is a kind this subset does not model" T-LABEL
   [: TJ-CHAR ;] E-HIR-KIND TTHROWSQ ;

\ ---- the schema table this dialect may fill ----------------------------------
\ A schema table names its dialect and its schema version in its header, fixed
\ when the module was created. HIR:NEW-BUILDER writes this dialect's own; a
\ module created through IR-BUILD directly can be given any, and registering
\ these opcodes into one of those is refused rather than quietly accepted.
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
\ Belonging to this module and existing in it are two different facts. Each
\ identity below carries the right owning module and names an ordinal the
\ interner never minted, so only the interner itself can refuse it - which is
\ what every declarer now asks it.
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
\ The elaborator holds both halves of one module at once: the source tape it
\ walks and the word model it asks what each token means. Before IR-BUILD had
\ live readers those two could not be the same module - a tape needed the
\ module's source registry and symbol rows, and a module under construction
\ hands out neither - so a fixture like this one had to build the tape against a
\ hand-made module and the schema against a real one. Here the source, the
\ spellings, the schema table, the word model and the tape all belong to a
\ single IR-BUILD module, and the join is what makes the lookups below work: a
\ token's spelling is an identity of the same interner the model declared its
\ rows from.
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
   v key r 0 HIR-WORD:ADMIT-TOKEN HIR-MEANING:LITERAL HIR-MEANING:EQ
   v key r 1 HIR-WORD:ADMIT-TOKEN HIR-MEANING:RENAME HIR-MEANING:EQ
   v key r 2 HIR-WORD:ADMIT-TOKEN HIR-MEANING:OP HIR-MEANING:EQ
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
\ Grouped harness contexts for the same reason as the tape and immediate suites:
\ a fixture that throws holds its context until the enclosing harness exits, so
\ the throwing groups are kept small and every one of their fixtures builds a
\ light word model rather than a whole module of this dialect.
: GROUP-DIALECT ( IR-CTX:ctx -- )
   drop
   COUNT-CASE
   NEWOPS-CASE
   NAMED-CASE ;

: GROUP-SHAPE ( IR-CTX:ctx -- )
   drop
   ARITH-CASE
   SHAPE-CASE
   MEM-SHAPE-CASE
   SPELL-CASE
   RULE-CASE ;

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
   OPS-CASE
   FLOAT-OPS-CASE
   FCMP-OPS-CASE
   FCMP-ABSENT-CASE
   OPS2-CASE
   CTRL-CASE
   STEP2-CASE
   DROP2-CASE
   MEMWORD-CASE
   CALLABLE-CASE
   RENAME-CASE
   MEAN-CASE
   LOCALS-MEAN-CASE
   MODELS-CASE
   ANN-CASE
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
   TJ-REFUSE-CASES ;

: GROUP-DIALECT-REFUSE ( IR-CTX:ctx -- )
   drop
   DIALECT-REFUSE-CASES ;

: GROUP-INTERNER ( IR-CTX:ctx -- )
   drop
   INTERNER-CASES ;

: GROUP-JOINED ( IR-CTX:ctx -- )
   drop
   JOINED-CASE ;

public

: RUN ( -- )
   T-RESET
   BND [: GROUP-DIALECT ;] IR-CTX:WITH-CONTEXT
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
   BND [: GROUP-INTERNER ;] IR-CTX:WITH-CONTEXT
   BND [: GROUP-JOINED ;] IR-CTX:WITH-CONTEXT
   CHECKER-CASES
   T-REPORT ;

;package

HIR-TEST:RUN

\ ir-schema.f - checked compiler dialect schema-table tests.
\
\ Proves the schema contract of src/compiler/ir/schema.f: a defined opcode
\ reads back exactly what was declared through every reader; a second
\ definition of the same opcode name rejects; an opcode name no schema defines
\ names nothing, and a row appended past this package's constructors still
\ cannot smuggle a value outside a closed vocabulary through a decoder; a
\ schema that never declared one of its required fields rejects, one fixture
\ per field; illegal arities, illegal effect combinations, illegal terminator
\ shapes, and illegal target combinations each reject with their own named
\ code; the record and table digests are deterministic, a presented digest that
\ does not match rejects, and varying exactly one field of a schema - or the
\ dialect its table belongs to - moves its digest; cross-owner keys, symbols,
\ types, and identities from another context reject named; capacities reject at
\ the committed ceilings; a frozen module serves every reader through the arena
\ views while the retired builders reject; context teardown releases everything;
\ and checker fixtures prove the API stays sealed.

require lib/test.f
require test/checker-assert.f
require test/compiler/ir-starve.f
require src/compiler/ir/schema.f

package IR-SCHEMA-TEST
private

\ The row shape src/compiler/ir/schema.f commits to, mirrored here so a fixture
\ can append a raw row past that package's constructors and prove the decoders
\ still hold. A change to the layout must change this mirror too.
24 constant ROW-CELLS
0 constant F-NAME
3 constant F-OPTAIL
6 constant F-RSTAIL
9 constant F-ATST
11 constant F-ATEXT
12 constant F-EFF
13 constant F-DOM
14 constant F-SPC
15 constant F-ALI
16 constant F-TRAP
17 constant F-ARCH
18 constant F-FEAT
19 constant F-TERM
20 constant F-RULE
1 constant F-OPST
2 constant F-OPN
23 constant F-TIN
-1 constant NO-POISON

\ ---- bindings ----------------------------------------------------------------
\ An AArch64 Darwin contract with the baseline instruction set and plain
\ floating point.
: BND ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ A PTX kernel contract: the schemas that reject above are declarable here,
\ because target legality is the binding's fact.
: PBND ( -- CBIND:binding )
   CTARGET-ARCH:PTX CTARGET-ABI:PTX-KERNEL CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ ---- module rigging ----------------------------------------------------------
: SYM-NEW ( IR-CTX:ctx IR-ID:ir-module-key -- IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key :}
   c key 16 256 IR-SYM:NEW ;

: TYP-NEW ( IR-CTX:ctx IR-ID:ir-module-key -- IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx key:IR-ID:ir-module-key :}
   c key 16 64 IR-TYPE:NEW ;

: I64 ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-ID:ir-type-id )
   IR--TYPE-WIDTH:W64 IR--TYPE-SIGN:SIGNED IR-TYPE:INT ;

: U8 ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key -- IR-ID:ir-type-id )
   IR--TYPE-WIDTH:W8 IR--TYPE-SIGN:UNSIGNED IR-TYPE:INT ;

\ ---- the field-variation vocabulary ------------------------------------------
\ Every fixture below stages one schema. V-BASE is the value-producing base;
\ V-BASE-C is the control base, a terminator with no results, because a
\ terminator may not have results and a non-terminator may not have successors,
\ so the successor and terminator variations are measured against it. Every
\ other selector changes exactly one field of its base.
0 constant V-BASE
1 constant V-NAME
2 constant V-OPTYPE
3 constant V-OPN
4 constant V-OPTAIL
5 constant V-RSTYPE
6 constant V-RSN
7 constant V-RSTAIL
8 constant V-REGN
9 constant V-ATTR
10 constant V-ATN
11 constant V-ATEXT
12 constant V-EFF
13 constant V-DOM
14 constant V-SPC
15 constant V-ALI
16 constant V-TRAP
17 constant V-ARCH
18 constant V-FEAT
19 constant V-RULE
20 constant V-REND
21 constant V-DIALECT
22 constant V-BASE-C
23 constant V-SUCC
24 constant V-TERM
25 constant V-TIE

: CONTROL-BASE? ( n -- bool )
   {: v:n :}
   v V-BASE-C = v V-SUCC = or v V-TERM = or ;

: DIA-SYM ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx sp:IR-ARENA:arena sr:IR-ARENA:arena key:IR-ID:ir-module-key v:n :}
   v V-DIALECT = if c sp sr key s" sir" IR-SYM:INTERN exit then
   c sp sr key s" hir" IR-SYM:INTERN ;

: OP-SYM ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx sp:IR-ARENA:arena sr:IR-ARENA:arena key:IR-ID:ir-module-key v:n :}
   v V-NAME = if c sp sr key s" hir.sub" IR-SYM:INTERN exit then
   c sp sr key s" hir.add" IR-SYM:INTERN ;

: RULE-SYM ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx sp:IR-ARENA:arena sr:IR-ARENA:arena key:IR-ID:ir-module-key v:n :}
   v V-RULE = if c sp sr key s" rule.sub" IR-SYM:INTERN exit then
   c sp sr key s" rule.add" IR-SYM:INTERN ;

: REND-SYM ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx sp:IR-ARENA:arena sr:IR-ARENA:arena key:IR-ID:ir-module-key v:n :}
   v V-REND = if c sp sr key s" render.sub" IR-SYM:INTERN exit then
   c sp sr key s" render.add" IR-SYM:INTERN ;

: STAGE-OPS ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx tp:IR-ARENA:arena tr:IR-ARENA:arena key:IR-ID:ir-module-key v:n :}
   v V-OPTYPE = if c tp tr key U8 IR-SCHEMA:ADD-OPERAND exit then
   v V-OPTAIL = if c tp tr key I64 IR-SCHEMA:ADD-OPERAND-TAIL exit then
   c tp tr key I64 IR-SCHEMA:ADD-OPERAND
   v V-OPN = if c tp tr key I64 IR-SCHEMA:ADD-OPERAND then ;

: STAGE-RES ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx tp:IR-ARENA:arena tr:IR-ARENA:arena key:IR-ID:ir-module-key v:n :}
   v CONTROL-BASE? if exit then
   v V-RSTYPE = if c tp tr key U8 IR-SCHEMA:ADD-RESULT exit then
   v V-RSTAIL = if c tp tr key I64 IR-SCHEMA:ADD-RESULT-TAIL exit then
   c tp tr key I64 IR-SCHEMA:ADD-RESULT
   v V-RSN = if c tp tr key I64 IR-SCHEMA:ADD-RESULT then ;

: STAGE-ATTRS ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx sp:IR-ARENA:arena sr:IR-ARENA:arena key:IR-ID:ir-module-key v:n :}
   v V-ATTR = if c sp sr key s" attr.beta" IR-SYM:INTERN IR-SCHEMA:ADD-ATTR exit then
   c sp sr key s" attr.alpha" IR-SYM:INTERN IR-SCHEMA:ADD-ATTR
   v V-ATN = if c sp sr key s" attr.beta" IR-SYM:INTERN IR-SCHEMA:ADD-ATTR then
   v V-ATEXT = if IR-SCHEMA:ADD-ATTR-EXT then ;

\ The base declares one operand and one result of the same type, so the tie
\ variation is the base with that one pair declared as one register field.
: STAGE-TIES ( n -- )
   {: v:n :}
   v V-TIE = if 0 0 IR-SCHEMA:ADD-TIE then ;

: STAGE-CONTROL ( n -- )
   {: v:n :}
   v V-SUCC = if true 1 0 IR-SCHEMA:SET-CONTROL exit then
   v V-TERM = if false 0 0 IR-SCHEMA:SET-CONTROL exit then
   v V-BASE-C = if true 0 0 IR-SCHEMA:SET-CONTROL exit then
   v V-REGN = if false 0 2 IR-SCHEMA:SET-CONTROL exit then
   false 0 0 IR-SCHEMA:SET-CONTROL ;

: STAGE-EFFECT ( n -- )
   {: v:n :}
   v V-EFF = if
      IR--TYPE-SPACE:GLOBAL IR--SCHEMA-ALIAS:UNALIASED IR--SCHEMA-EFFECT:WRITE
      IR-SCHEMA:SET-MEMORY exit then
   v V-SPC = if
      IR--TYPE-SPACE:SHARED IR--SCHEMA-ALIAS:UNALIASED IR--SCHEMA-EFFECT:READ
      IR-SCHEMA:SET-MEMORY exit then
   v V-ALI = if
      IR--TYPE-SPACE:GLOBAL IR--SCHEMA-ALIAS:GROUPED IR--SCHEMA-EFFECT:READ
      IR-SCHEMA:SET-MEMORY exit then
   v V-DOM = if
      IR--TYPE-DOMAIN:IO IR--SCHEMA-EFFECT:READ IR-SCHEMA:SET-TOKEN exit then
   IR--TYPE-SPACE:GLOBAL IR--SCHEMA-ALIAS:UNALIASED IR--SCHEMA-EFFECT:READ
   IR-SCHEMA:SET-MEMORY ;

: STAGE-TARGET ( n -- )
   {: v:n :}
   v V-ARCH = if CTARGET-ARCH:PTX CTARGET:F-BASE IR-SCHEMA:SET-TARGET exit then
   v V-FEAT = if
      CTARGET-ARCH:AARCH64 CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH
      IR-SCHEMA:SET-TARGET exit then
   CTARGET-ARCH:AARCH64 CTARGET:F-BASE IR-SCHEMA:SET-TARGET ;

\ Stage one whole schema for the chosen variation.
: STAGE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena key:IR-ID:ir-module-key v:n :}
   c sp sr key v OP-SYM IR-SCHEMA:BEGIN-OP
   c tp tr key v STAGE-OPS
   c tp tr key v STAGE-RES
   c sp sr key v STAGE-ATTRS
   v STAGE-TIES
   v STAGE-CONTROL
   v STAGE-EFFECT
   v V-TRAP = IR-SCHEMA:SET-TRAP
   v STAGE-TARGET
   c sp sr key v RULE-SYM IR-SCHEMA:SET-RULE
   c sp sr key v REND-SYM IR-SCHEMA:SET-RENDERER ;

: TAB-NEW ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx sp:IR-ARENA:arena sr:IR-ARENA:arena key:IR-ID:ir-module-key v:n :}
   c sr key  c sp sr key v DIA-SYM  1 0 8 64 IR-SCHEMA:NEW ;

\ ---- a definition is one commit, at the scratch edge ---------------------------
\ Defining an opcode writes four pool windows and then a twenty-four-cell row.
\ Each append grows its arena by taking a span from the context mapping, so
\ defining at the edge of that mapping used to leave the windows in and the row
\ half written - a cell count twenty-four does not divide, and the dialect's
\ whole schema table becomes unreadable. Both arenas are now reserved before
\ the first of them is touched.
\
\ One opcode already sits in the table, at thirty of thirty-two cells, so the
\ second needs a doubled span and cannot have one. If that stops being true the
\ definition succeeds, its code is zero, and this case fails.

\ Depth-neutral: the six inputs ride beneath the definition that fails.
: TORN-DEF ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena -- IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena )
   {: c:IR-CTX:ctx a:IR-ARENA:arena r:IR-ARENA:arena key:IR-ID:ir-module-key sr:IR-ARENA:arena tr:IR-ARENA:arena :}
   c a r key sr tr
   c a r key sr tr IR-SCHEMA:DEFINE ;

: TORN-BODY ( IR-CTX:ctx -- n n bool n n )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: tp:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c sp sr tp tr key V-BASE STAGE
   c a r key sr tr IR-SCHEMA:DEFINE
   a IR-ARENA:USED {: pcells:n :}
   r IR-ARENA:USED {: rcells:n :}
   c sp sr tp tr key V-NAME STAGE
   c IR-STARVE:EDGE
   c a r key sr tr [: TORN-DEF ;] catch
   {: c2:IR-CTX:ctx a2:IR-ARENA:arena r2:IR-ARENA:arena key2:IR-ID:ir-module-key sr2:IR-ARENA:arena tr2:IR-ARENA:arena rc:n :}
   rc
   r2  c sp sr key V-BASE OP-SYM  IR-SCHEMA:OPERANDS
   r2  c sp sr key V-NAME OP-SYM  IR-SCHEMA:DEFINED?
   a2 IR-ARENA:USED pcells -
   r2 IR-ARENA:USED rcells - ;

: TORN-CASE ( -- )
   s" a definition refused for want of scratch leaves both stores whole" T-LABEL
   BND [: TORN-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 0 T= TFALSE 1 T= E-IR-CTX-SCRATCH T= ;

\ ---- one variation, built and digested ---------------------------------------
: DIG ( IR-CTX:ctx n -- CDIGEST:digest )
   {: c:IR-CTX:ctx v:n :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: tp:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sp sr key v TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c sp sr tp tr key v STAGE
   c a r key sr tr IR-SCHEMA:DEFINE
   a r  c sp sr key v OP-SYM  IR-SCHEMA:DIGEST ;

: DIG1 ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- CDIGEST:digest )
   {: c:IR-CTX:ctx sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena key:IR-ID:ir-module-key v:n :}
   c sp sr key v TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c sp sr tp tr key v STAGE
   c a r key sr tr IR-SCHEMA:DEFINE
   a r  c sp sr key v OP-SYM  IR-SCHEMA:DIGEST ;

\ A table holding the base schema, and optionally one more opcode.
: TAB-DIG ( n IR-CTX:ctx -- CDIGEST:digest )
   {: extra:n c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: tp:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c sp sr tp tr key V-BASE STAGE
   c a r key sr tr IR-SCHEMA:DEFINE
   extra 0<> if
      c sp sr tp tr key V-NAME STAGE
      c a r key sr tr IR-SCHEMA:DEFINE
   then
   a r IR-SCHEMA:TABLE-DIGEST ;

\ ---- reading one defined schema back -----------------------------------------
: READ-BODY ( IR-CTX:ctx -- n bool n bool n n n bool bool n n )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: tp:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c sp sr tp tr key V-BASE STAGE
   c a r key sr tr IR-SCHEMA:DEFINE
   c sp sr key V-BASE OP-SYM {: op:IR-ID:ir-symbol-id :}
   r IR-SCHEMA:SCHEMAS
   r op IR-SCHEMA:DEFINED?
   r op IR-SCHEMA:OPERANDS
   r op IR-SCHEMA:OPERAND-TAIL?
   r op IR-SCHEMA:RESULTS
   r op IR-SCHEMA:SUCCESSORS
   r op IR-SCHEMA:REGIONS
   r op IR-SCHEMA:TRAPS?
   r op IR-SCHEMA:TERMINATOR?
   r op IR-SCHEMA:ATTRS
   r op IR-SCHEMA:TIES ;

: READ-CASE ( -- )
   s" a defined schema reads back the counts and flags it declared" T-LABEL
   BND [: READ-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 1 T= TFALSE TFALSE 0 T= 0 T= 1 T= TFALSE 1 T= TTRUE 1 T= ;

: FIELD-BODY ( IR-CTX:ctx -- bool bool bool bool bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: tp:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c sp sr tp tr key V-BASE STAGE
   c a r key sr tr IR-SCHEMA:DEFINE
   c sp sr key V-BASE OP-SYM {: op:IR-ID:ir-symbol-id :}
   r op IR-SCHEMA:EFFECT@ IR--SCHEMA-EFFECT:READ IR--SCHEMA-EFFECT:EQ
   r op IR-SCHEMA:DOMAIN@ IR--TYPE-DOMAIN:DATA-MEM IR--TYPE-DOMAIN:EQ
   r op IR-SCHEMA:SPACE@ IR--TYPE-SPACE:GLOBAL IR--TYPE-SPACE:EQ
   r op IR-SCHEMA:ALIAS@ IR--SCHEMA-ALIAS:UNALIASED IR--SCHEMA-ALIAS:EQ
   r op IR-SCHEMA:ARCH@ CTARGET-ARCH:AARCH64 CTARGET-ARCH:EQ
   a r key op 0 IR-SCHEMA:OPERAND@ IR-ID:TYPE-LOCAL
      c tp tr key I64 IR-ID:TYPE-LOCAL =
   a r key op 0 IR-SCHEMA:RESULT@ IR-ID:TYPE-LOCAL
      c tp tr key I64 IR-ID:TYPE-LOCAL =
   a r key op 0 IR-SCHEMA:ATTR@ IR-ID:SYMBOL-LOCAL
      c sp sr key s" attr.alpha" IR-SYM:INTERN IR-ID:SYMBOL-LOCAL = ;

: FIELD-CASE ( -- )
   s" every typed field reads back the value it was declared with" T-LABEL
   BND [: FIELD-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE TTRUE ;

: HDR-BODY ( IR-CTX:ctx -- n n bool )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   r IR-SCHEMA:MAJOR@
   r IR-SCHEMA:MINOR@
   r key IR-SCHEMA:DIALECT@ IR-ID:SYMBOL-LOCAL
      c sp sr key s" hir" IR-SYM:INTERN IR-ID:SYMBOL-LOCAL = ;

: HDR-CASE ( -- )
   s" the table header carries its dialect and schema version" T-LABEL
   BND [: HDR-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 0 T= 1 T= ;

\ ---- duplicate and unknown opcodes -------------------------------------------
: DUP-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: tp:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c sp sr tp tr key V-BASE STAGE
   c a r key sr tr IR-SCHEMA:DEFINE
   c sp sr tp tr key V-BASE STAGE
   c a r key sr tr IR-SCHEMA:DEFINE ;

: DUP-RUN ( -- )
   BND [: DUP-BODY ;] IR-CTX:WITH-CONTEXT ;

: UNKNOWN-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: tp:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c sp sr tp tr key V-BASE STAGE
   c a r key sr tr IR-SCHEMA:DEFINE
   r  c sp sr key s" hir.ghost" IR-SYM:INTERN  IR-SCHEMA:EFFECT@ drop ;

: UNKNOWN-RUN ( -- )
   BND [: UNKNOWN-BODY ;] IR-CTX:WITH-CONTEXT ;

: UNKNOWN-DEFINED-BODY ( IR-CTX:ctx -- bool bool )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: tp:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c sp sr tp tr key V-BASE STAGE
   c a r key sr tr IR-SCHEMA:DEFINE
   r  c sp sr key V-BASE OP-SYM  IR-SCHEMA:DEFINED?
   r  c sp sr key s" hir.ghost" IR-SYM:INTERN  IR-SCHEMA:DEFINED? ;

: OPCODE-CASES ( -- )
   s" a second definition of one opcode name rejects" T-LABEL
   [: DUP-RUN ;] E-IR-SCHEMA-DUP TTHROWSQ
   s" an opcode name no schema defines rejects at the reader" T-LABEL
   [: UNKNOWN-RUN ;] E-IR-SCHEMA-OPCODE TTHROWSQ
   s" DEFINED? separates a defined opcode from an undefined name" T-LABEL
   BND [: UNKNOWN-DEFINED-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TTRUE ;

\ ---- incomplete schemas ------------------------------------------------------
\ One fixture per required field: every other field is declared, the named one
\ is left out, and the definition rejects.
: MISS-STAGE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx sp:IR-ARENA:arena sr:IR-ARENA:arena key:IR-ID:ir-module-key skip:n :}
   c sp sr key V-BASE OP-SYM IR-SCHEMA:BEGIN-OP
   skip 1 <> if false 0 0 IR-SCHEMA:SET-CONTROL then
   skip 2 <> if IR-SCHEMA:SET-PURE then
   skip 3 <> if false IR-SCHEMA:SET-TRAP then
   skip 4 <> if CTARGET-ARCH:AARCH64 CTARGET:F-BASE IR-SCHEMA:SET-TARGET then
   skip 5 <> if c sp sr key V-BASE RULE-SYM IR-SCHEMA:SET-RULE then
   skip 6 <> if c sp sr key V-BASE REND-SYM IR-SCHEMA:SET-RENDERER then ;

: MISS-BODY ( IR-CTX:ctx n -- )
   {: c:IR-CTX:ctx skip:n :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: tp:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c sp sr key skip MISS-STAGE
   c a r key sr tr IR-SCHEMA:DEFINE ;

: MISS-CONTROL ( -- )   BND [: 1 MISS-BODY ;] IR-CTX:WITH-CONTEXT ;
: MISS-EFFECT ( -- )    BND [: 2 MISS-BODY ;] IR-CTX:WITH-CONTEXT ;
: MISS-TRAP ( -- )      BND [: 3 MISS-BODY ;] IR-CTX:WITH-CONTEXT ;
: MISS-TARGET ( -- )    BND [: 4 MISS-BODY ;] IR-CTX:WITH-CONTEXT ;
: MISS-RULE ( -- )      BND [: 5 MISS-BODY ;] IR-CTX:WITH-CONTEXT ;
: MISS-REND ( -- )      BND [: 6 MISS-BODY ;] IR-CTX:WITH-CONTEXT ;
: MISS-NONE ( -- )      BND [: 0 MISS-BODY ;] IR-CTX:WITH-CONTEXT ;

: NO-BEGIN-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: tp:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c a r key sr tr IR-SCHEMA:DEFINE ;

: NO-BEGIN-RUN ( -- )
   BND [: NO-BEGIN-BODY ;] IR-CTX:WITH-CONTEXT ;

: FIELD-CASES ( -- )
   s" a schema with every required field declared defines" T-LABEL
   MISS-NONE
   s" a schema with no control-flow shape rejects" T-LABEL
   [: MISS-CONTROL ;] E-IR-SCHEMA-FIELD TTHROWSQ
   s" a schema with no effect class rejects" T-LABEL
   [: MISS-EFFECT ;] E-IR-SCHEMA-FIELD TTHROWSQ
   s" a schema with no trap flag rejects" T-LABEL
   [: MISS-TRAP ;] E-IR-SCHEMA-FIELD TTHROWSQ
   s" a schema with no target requirement rejects" T-LABEL
   [: MISS-TARGET ;] E-IR-SCHEMA-FIELD TTHROWSQ
   s" a schema with no semantic rule rejects" T-LABEL
   [: MISS-RULE ;] E-IR-SCHEMA-FIELD TTHROWSQ
   s" a schema with no renderer rejects" T-LABEL
   [: MISS-REND ;] E-IR-SCHEMA-FIELD TTHROWSQ
   s" defining without an open schema rejects" T-LABEL
   [: NO-BEGIN-RUN ;] E-IR-SCHEMA-STAGE TTHROWSQ ;

\ ---- stage protocol ----------------------------------------------------------
: OPEN-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c sp sr key V-BASE OP-SYM IR-SCHEMA:BEGIN-OP
   c sp sr key V-BASE OP-SYM IR-SCHEMA:BEGIN-OP ;

: TWICE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c sp sr key V-BASE OP-SYM IR-SCHEMA:BEGIN-OP
   IR-SCHEMA:SET-PURE
   IR-SCHEMA:SET-PURE ;

: EXT-TWICE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c sp sr key V-BASE OP-SYM IR-SCHEMA:BEGIN-OP
   IR-SCHEMA:ADD-ATTR-EXT
   IR-SCHEMA:ADD-ATTR-EXT ;

: LOOSE-BODY ( IR-CTX:ctx -- )
   drop
   IR-SCHEMA:SET-PURE ;

\ A rejected end still consumes the stage: the schema below is opened and its
\ definition rejected for a missing field, and the next definition still lands.

: ABANDON-BODY ( IR-CTX:ctx -- n )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: tp:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c sp sr key V-BASE OP-SYM IR-SCHEMA:BEGIN-OP
   IR-SCHEMA:ABANDON
   c sp sr tp tr key V-BASE STAGE
   c a r key sr tr IR-SCHEMA:DEFINE
   r IR-SCHEMA:SCHEMAS ;

\ A throw inside a staged declaration leaves the schema open, because only an
\ end - DEFINE or ABANDON - consumes the stage. The next BEGIN-OP therefore
\ rejects until the caller abandons, which is what this does between fixtures.
: CLEAR-STAGE ( -- )
   [: IR-SCHEMA:ABANDON ;] catch drop ;

: OPEN-RUN ( -- )       BND [: OPEN-BODY ;] IR-CTX:WITH-CONTEXT ;
: TWICE-RUN ( -- )      BND [: TWICE-BODY ;] IR-CTX:WITH-CONTEXT ;
: EXT-TWICE-RUN ( -- )  BND [: EXT-TWICE-BODY ;] IR-CTX:WITH-CONTEXT ;
: LOOSE-RUN ( -- )      BND [: LOOSE-BODY ;] IR-CTX:WITH-CONTEXT ;

: STAGE-CASES ( -- )
   s" declaring a field with no schema open rejects" T-LABEL
   [: LOOSE-RUN ;] E-IR-SCHEMA-STAGE TTHROWSQ
   s" a rejected definition consumes the stage" T-LABEL
   [: MISS-CONTROL ;] E-IR-SCHEMA-FIELD TTHROWSQ
   BND [: READ-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 1 T= TFALSE TFALSE 0 T= 0 T= 1 T= TFALSE 1 T= TTRUE 1 T=
   s" abandoning a schema leaves the next one free to open" T-LABEL
   BND [: ABANDON-BODY ;] IR-CTX:WITH-CONTEXT 1 T=
   s" opening a schema while one is open rejects" T-LABEL
   [: OPEN-RUN ;] E-IR-SCHEMA-STAGE TTHROWSQ
   s" a schema left open by a throw blocks the next one until abandoned" T-LABEL
   [: OPEN-RUN ;] E-IR-SCHEMA-STAGE TTHROWSQ
   CLEAR-STAGE
   BND [: ABANDON-BODY ;] IR-CTX:WITH-CONTEXT 1 T=
   s" declaring one field twice rejects" T-LABEL
   [: TWICE-RUN ;] E-IR-SCHEMA-STAGE TTHROWSQ
   CLEAR-STAGE
   s" permitting the extension set twice rejects" T-LABEL
   [: EXT-TWICE-RUN ;] E-IR-SCHEMA-STAGE TTHROWSQ
   CLEAR-STAGE ;

\ ---- illegal arities ---------------------------------------------------------
: ARITY-STAGE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena key:IR-ID:ir-module-key k:n :}
   c sp sr key V-BASE OP-SYM IR-SCHEMA:BEGIN-OP
   k 1 = if
      33 0 ?do c tp tr key I64 IR-SCHEMA:ADD-OPERAND loop
   then
   k 2 = if
      c tp tr key I64 IR-SCHEMA:ADD-OPERAND-TAIL
      c tp tr key I64 IR-SCHEMA:ADD-OPERAND
   then
   k 3 = if
      c tp tr key I64 IR-SCHEMA:ADD-OPERAND-TAIL
      c tp tr key I64 IR-SCHEMA:ADD-OPERAND-TAIL
   then
   k 4 = if
      c tp tr key I64 IR-SCHEMA:ADD-RESULT-TAIL
      c tp tr key I64 IR-SCHEMA:ADD-RESULT
   then
   k 5 = if true 33 0 IR-SCHEMA:SET-CONTROL then
   k 6 = if false 0 9 IR-SCHEMA:SET-CONTROL then
   k 7 = if true -1 0 IR-SCHEMA:SET-CONTROL then ;

: ARITY-BODY ( n IR-CTX:ctx -- )
   {: k:n c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: tp:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c sp sr tp tr key k ARITY-STAGE
   IR-SCHEMA:ABANDON ;

: ARITY-RUN ( n -- )
   BND [: ARITY-BODY ;] IR-CTX:WITH-CONTEXT ;

\ SET-CONTROL stores its counts and DEFINE checks them, so the two ceiling
\ fixtures reject at the definition rather than at the declaration.
: CTRL-BODY ( n IR-CTX:ctx -- )
   {: k:n c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: tp:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c sp sr key V-BASE OP-SYM IR-SCHEMA:BEGIN-OP
   k 5 = if true 33 0 IR-SCHEMA:SET-CONTROL then
   k 6 = if false 0 9 IR-SCHEMA:SET-CONTROL then
   k 7 = if true -1 0 IR-SCHEMA:SET-CONTROL then
   IR-SCHEMA:SET-PURE
   false IR-SCHEMA:SET-TRAP
   CTARGET-ARCH:AARCH64 CTARGET:F-BASE IR-SCHEMA:SET-TARGET
   c sp sr key V-BASE RULE-SYM IR-SCHEMA:SET-RULE
   c sp sr key V-BASE REND-SYM IR-SCHEMA:SET-RENDERER
   c a r key sr tr IR-SCHEMA:DEFINE ;

: CTRL-RUN ( n -- )
   BND [: CTRL-BODY ;] IR-CTX:WITH-CONTEXT ;

: ARITY-CASES ( -- )
   s" an operand list past its ceiling rejects" T-LABEL
   [: 1 ARITY-RUN ;] E-IR-SCHEMA-ARITY TTHROWSQ
   CLEAR-STAGE
   s" an operand after the variadic tail rejects" T-LABEL
   [: 2 ARITY-RUN ;] E-IR-SCHEMA-ARITY TTHROWSQ
   CLEAR-STAGE
   s" a second operand tail rejects" T-LABEL
   [: 3 ARITY-RUN ;] E-IR-SCHEMA-ARITY TTHROWSQ
   CLEAR-STAGE
   s" a result after the variadic result tail rejects" T-LABEL
   [: 4 ARITY-RUN ;] E-IR-SCHEMA-ARITY TTHROWSQ
   CLEAR-STAGE
   s" a successor count past its ceiling rejects" T-LABEL
   [: 5 CTRL-RUN ;] E-IR-SCHEMA-ARITY TTHROWSQ
   s" a region count past its ceiling rejects" T-LABEL
   [: 6 CTRL-RUN ;] E-IR-SCHEMA-ARITY TTHROWSQ
   s" a negative successor count rejects" T-LABEL
   [: 7 CTRL-RUN ;] E-IR-SCHEMA-ARITY TTHROWSQ ;

\ ---- illegal effect combinations ---------------------------------------------
: EFF-BODY ( n IR-CTX:ctx -- )
   {: k:n c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c sp sr key V-BASE OP-SYM IR-SCHEMA:BEGIN-OP
   k 1 = if
      IR--TYPE-SPACE:GLOBAL IR--SCHEMA-ALIAS:UNALIASED IR--SCHEMA-EFFECT:PURE
      IR-SCHEMA:SET-MEMORY
   then
   k 2 = if
      IR--TYPE-DOMAIN:IO IR--SCHEMA-EFFECT:PURE IR-SCHEMA:SET-TOKEN
   then
   k 3 = if
      IR--TYPE-DOMAIN:DATA-MEM IR--SCHEMA-EFFECT:READ IR-SCHEMA:SET-TOKEN
   then
   IR-SCHEMA:ABANDON ;

: EFF-RUN ( n -- )
   BND [: EFF-BODY ;] IR-CTX:WITH-CONTEXT ;

: SHAPE-BODY ( IR-CTX:ctx n -- )
   {: c:IR-CTX:ctx k:n :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: tp:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c sp sr tp tr key V-DOM STAGE
   c a r key sr tr IR-SCHEMA:DEFINE
   c sp sr key V-DOM OP-SYM {: op:IR-ID:ir-symbol-id :}
   k 1 = if r op IR-SCHEMA:SPACE@ drop then
   k 2 = if r op IR-SCHEMA:ALIAS@ drop then ;

: PURE-DOM-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: tp:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c sp sr key V-BASE OP-SYM IR-SCHEMA:BEGIN-OP
   false 0 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   false IR-SCHEMA:SET-TRAP
   CTARGET-ARCH:AARCH64 CTARGET:F-BASE IR-SCHEMA:SET-TARGET
   c sp sr key V-BASE RULE-SYM IR-SCHEMA:SET-RULE
   c sp sr key V-BASE REND-SYM IR-SCHEMA:SET-RENDERER
   c a r key sr tr IR-SCHEMA:DEFINE
   r  c sp sr key V-BASE OP-SYM  IR-SCHEMA:DOMAIN@ drop ;

: SPACE-RUN ( -- )     BND [: 1 SHAPE-BODY ;] IR-CTX:WITH-CONTEXT ;
: ALIAS-RUN ( -- )     BND [: 2 SHAPE-BODY ;] IR-CTX:WITH-CONTEXT ;
: PURE-DOM-RUN ( -- )  BND [: PURE-DOM-BODY ;] IR-CTX:WITH-CONTEXT ;

: EFFECT-CASES ( -- )
   s" a pure class presented to the memory effect rejects" T-LABEL
   [: 1 EFF-RUN ;] E-IR-SCHEMA-EFFECT TTHROWSQ
   CLEAR-STAGE
   s" a pure class presented to a token effect rejects" T-LABEL
   [: 2 EFF-RUN ;] E-IR-SCHEMA-EFFECT TTHROWSQ
   CLEAR-STAGE
   s" the data-memory domain presented as a bare token rejects" T-LABEL
   [: 3 EFF-RUN ;] E-IR-SCHEMA-EFFECT TTHROWSQ
   CLEAR-STAGE
   s" asking a token schema for its memory space rejects" T-LABEL
   [: SPACE-RUN ;] E-IR-SCHEMA-KIND TTHROWSQ
   s" asking a token schema for its alias behaviour rejects" T-LABEL
   [: ALIAS-RUN ;] E-IR-SCHEMA-KIND TTHROWSQ
   s" asking a pure schema for its effect domain rejects" T-LABEL
   [: PURE-DOM-RUN ;] E-IR-SCHEMA-KIND TTHROWSQ ;

\ ---- terminator rules --------------------------------------------------------
: TERM-BODY ( n IR-CTX:ctx -- )
   {: k:n c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: tp:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c sp sr key V-BASE OP-SYM IR-SCHEMA:BEGIN-OP
   k 1 = if false 1 0 IR-SCHEMA:SET-CONTROL then
   k 2 = if
      true 1 0 IR-SCHEMA:SET-CONTROL
      c tp tr key I64 IR-SCHEMA:ADD-RESULT
   then
   k 3 = if true 2 0 IR-SCHEMA:SET-CONTROL then
   IR-SCHEMA:SET-PURE
   false IR-SCHEMA:SET-TRAP
   CTARGET-ARCH:AARCH64 CTARGET:F-BASE IR-SCHEMA:SET-TARGET
   c sp sr key V-BASE RULE-SYM IR-SCHEMA:SET-RULE
   c sp sr key V-BASE REND-SYM IR-SCHEMA:SET-RENDERER
   c a r key sr tr IR-SCHEMA:DEFINE ;

: TERM-RUN ( n -- )
   BND [: TERM-BODY ;] IR-CTX:WITH-CONTEXT ;

: TERM-CASES ( -- )
   s" a non-terminator with successors rejects" T-LABEL
   [: 1 TERM-RUN ;] E-IR-SCHEMA-TERM TTHROWSQ
   s" a terminator with results rejects" T-LABEL
   [: 2 TERM-RUN ;] E-IR-SCHEMA-TERM TTHROWSQ
   s" a terminator with successors and no results defines" T-LABEL
   3 TERM-RUN ;

\ ---- tied operands -----------------------------------------------------------
\ One fixture per way a tie can be wrong, each staged inside a schema that is
\ legal apart from the tie, so the refusal is the tie's own. Case 0 is the legal
\ one: one operand and one result of the same type, declared as one register
\ field.
: TIE-OPS ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx tp:IR-ARENA:arena tr:IR-ARENA:arena key:IR-ID:ir-module-key k:n :}
   k 6 = if c tp tr key I64 IR-SCHEMA:ADD-OPERAND-TAIL exit then
   k 7 = if c tp tr key U8 IR-SCHEMA:ADD-OPERAND exit then
   c tp tr key I64 IR-SCHEMA:ADD-OPERAND
   k 4 = if c tp tr key I64 IR-SCHEMA:ADD-OPERAND then ;

: TIE-RES ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx tp:IR-ARENA:arena tr:IR-ARENA:arena key:IR-ID:ir-module-key k:n :}
   k 8 = if exit then
   c tp tr key I64 IR-SCHEMA:ADD-RESULT
   k 5 = if c tp tr key I64 IR-SCHEMA:ADD-RESULT then ;

: TIE-CONTROL ( n -- )
   {: k:n :}
   k 8 = if
      true 0 0 IR-SCHEMA:SET-CONTROL exit
   then
   false 0 0 IR-SCHEMA:SET-CONTROL ;

: TIE-DECL ( n -- )
   {: k:n :}
   k 1 = if 1 0 IR-SCHEMA:ADD-TIE exit then
   k 2 = if 0 1 IR-SCHEMA:ADD-TIE exit then
   k 3 = if -1 0 IR-SCHEMA:ADD-TIE exit then
   k 4 = if
      0 0 IR-SCHEMA:ADD-TIE
      0 1 IR-SCHEMA:ADD-TIE exit
   then
   k 5 = if
      0 0 IR-SCHEMA:ADD-TIE
      1 0 IR-SCHEMA:ADD-TIE exit
   then
   0 0 IR-SCHEMA:ADD-TIE ;

: TIE-STAGE ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key n -- )
   {: c:IR-CTX:ctx sp:IR-ARENA:arena sr:IR-ARENA:arena tp:IR-ARENA:arena tr:IR-ARENA:arena key:IR-ID:ir-module-key k:n :}
   c sp sr key V-BASE OP-SYM IR-SCHEMA:BEGIN-OP
   c tp tr key k TIE-OPS
   c tp tr key k TIE-RES
   k TIE-DECL
   k TIE-CONTROL
   IR-SCHEMA:SET-PURE
   false IR-SCHEMA:SET-TRAP
   CTARGET-ARCH:AARCH64 CTARGET:F-BASE IR-SCHEMA:SET-TARGET
   c sp sr key V-BASE RULE-SYM IR-SCHEMA:SET-RULE
   c sp sr key V-BASE REND-SYM IR-SCHEMA:SET-RENDERER ;

: TIE-BODY ( n IR-CTX:ctx -- )
   {: k:n c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: tp:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c sp sr tp tr key k TIE-STAGE
   c a r key sr tr IR-SCHEMA:DEFINE ;

: TIE-RUN ( n -- )
   BND [: TIE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ The declared tie reads back through the live readers and, once the module is
\ frozen, through the view readers, which is the pair a register allocator uses.
: TIE-READ-BODY ( IR-CTX:ctx -- n n n n n n )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: tp:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c sp sr tp tr key 0 TIE-STAGE
   c a r key sr tr IR-SCHEMA:DEFINE
   c sp sr key V-BASE OP-SYM {: op:IR-ID:ir-symbol-id :}
   r op IR-SCHEMA:TIES
   a r op 0 IR-SCHEMA:TIE-RESULT@
   a r op 0 IR-SCHEMA:TIE-OPERAND@
   a IR-ARENA:FREEZE {: pv:IR-ARENA:view :}
   r IR-ARENA:FREEZE {: rv:IR-ARENA:view :}
   rv op IR-SCHEMA:FTIES
   pv rv op 0 IR-SCHEMA:FTIE-RESULT@
   pv rv op 0 IR-SCHEMA:FTIE-OPERAND@ ;

: TIE-IDX-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: tp:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c sp sr tp tr key 0 TIE-STAGE
   c a r key sr tr IR-SCHEMA:DEFINE
   a r  c sp sr key V-BASE OP-SYM  1 IR-SCHEMA:TIE-RESULT@ drop ;

: TIE-IDX-RUN ( -- )
   BND [: TIE-IDX-BODY ;] IR-CTX:WITH-CONTEXT ;

: TIE-CASES ( -- )
   s" a result tied to an operand of the same type defines" T-LABEL
   0 TIE-RUN
   s" the declared tie reads back live and through the frozen views" T-LABEL
   BND [: TIE-READ-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 0 T= 1 T= 0 T= 0 T= 1 T=
   s" a tie naming a result past the result list rejects" T-LABEL
   [: 1 TIE-RUN ;] E-IR-SCHEMA-TIE TTHROWSQ
   s" a tie naming an operand past the operand list rejects" T-LABEL
   [: 2 TIE-RUN ;] E-IR-SCHEMA-TIE TTHROWSQ
   s" a negative tied result ordinal rejects" T-LABEL
   [: 3 TIE-RUN ;] E-IR-SCHEMA-TIE TTHROWSQ
   s" tying one result to two operands rejects" T-LABEL
   [: 4 TIE-RUN ;] E-IR-SCHEMA-TIE TTHROWSQ
   s" tying two results to one operand rejects" T-LABEL
   [: 5 TIE-RUN ;] E-IR-SCHEMA-TIE TTHROWSQ
   s" a tie onto the variadic operand tail rejects" T-LABEL
   [: 6 TIE-RUN ;] E-IR-SCHEMA-TIE TTHROWSQ
   s" a tie between two different types rejects" T-LABEL
   [: 7 TIE-RUN ;] E-IR-SCHEMA-TIE TTHROWSQ
   s" a terminator tying anything rejects, having no results" T-LABEL
   [: 8 TIE-RUN ;] E-IR-SCHEMA-TIE TTHROWSQ
   s" a tie index past the declared ties rejects" T-LABEL
   [: TIE-IDX-RUN ;] E-IR-SCHEMA-BOUND TTHROWSQ ;

\ ---- target legality ---------------------------------------------------------
: TGT-BODY ( n IR-CTX:ctx -- )
   {: k:n c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: tp:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c sp sr key V-BASE OP-SYM IR-SCHEMA:BEGIN-OP
   false 0 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   false IR-SCHEMA:SET-TRAP
   k 1 = if CTARGET-ARCH:PTX CTARGET:F-BASE IR-SCHEMA:SET-TARGET then
   k 2 = if
      CTARGET-ARCH:AARCH64 CTARGET:F-BASE CTARGET:F-SIMD CTARGET:WITH
      IR-SCHEMA:SET-TARGET
   then
   k 3 = if
      CTARGET-ARCH:AARCH64 CTARGET:F-BASE CTARGET:F-MMA CTARGET:WITH
      IR-SCHEMA:SET-TARGET
   then
   k 4 = if CTARGET-ARCH:AARCH64 CTARGET:F-BASE IR-SCHEMA:SET-TARGET then
   c sp sr key V-BASE RULE-SYM IR-SCHEMA:SET-RULE
   c sp sr key V-BASE REND-SYM IR-SCHEMA:SET-RENDERER
   c a r key sr tr IR-SCHEMA:DEFINE ;

: TGT-RUN ( n -- )
   BND [: TGT-BODY ;] IR-CTX:WITH-CONTEXT ;

: PTGT-RUN ( n -- )
   PBND [: TGT-BODY ;] IR-CTX:WITH-CONTEXT ;

: TARGET-CASES ( -- )
   s" an architecture other than the bound one rejects" T-LABEL
   [: 1 TGT-RUN ;] E-IR-SCHEMA-TARGET TTHROWSQ
   s" a feature the bound contract does not have rejects" T-LABEL
   [: 2 TGT-RUN ;] E-IR-SCHEMA-TARGET TTHROWSQ
   s" a feature the required architecture cannot have rejects" T-LABEL
   [: 3 TGT-RUN ;] E-IR-SCHEMA-TARGET TTHROWSQ
   s" the same architecture requirement defines on the binding that states it" T-LABEL
   1 PTGT-RUN
   s" a requirement the bound contract satisfies defines" T-LABEL
   4 TGT-RUN ;

\ ---- digests -----------------------------------------------------------------
\ Both variants share one module, so a changed symbol or type reference really
\ is a changed module-local ordinal; two separate modules would hand the two
\ different strings the same insertion ordinal and hide the change.
: SAME-DIG? ( n n IR-CTX:ctx -- bool )
   {: x:n y:n c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: tp:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sp sr tp tr key x DIG1
   c sp sr tp tr key y DIG1
   CDIGEST-DIGEST:EQ ;

: DIG-MOVES ( n -- )
   V-BASE swap BND [: SAME-DIG? ;] IR-CTX:WITH-CONTEXT TFALSE ;

: CDIG-MOVES ( n -- )
   V-BASE-C swap BND [: SAME-DIG? ;] IR-CTX:WITH-CONTEXT TFALSE ;

\ The architecture field is measured across bindings, because a schema may only
\ require the architecture its context is bound to; the digest is a pure
\ function of the record, so the two halves compare across contexts.
: ARCH-DIG-CASE ( -- )
   s" the required architecture participates in the digest" T-LABEL
   BND [: V-BASE DIG ;] IR-CTX:WITH-CONTEXT
   PBND [: V-ARCH DIG ;] IR-CTX:WITH-CONTEXT
   CDIGEST-DIGEST:EQ TFALSE ;

: STABLE-CASE ( -- )
   s" the same schema digests to the same value twice" T-LABEL
   V-BASE V-BASE BND [: SAME-DIG? ;] IR-CTX:WITH-CONTEXT TTRUE ;

: TABLE-STABLE-BODY ( IR-CTX:ctx -- bool )
   dup 0 swap TAB-DIG  swap 0 swap TAB-DIG  CDIGEST-DIGEST:EQ ;

: TABLE-MOVES-BODY ( IR-CTX:ctx -- bool )
   dup 0 swap TAB-DIG  swap 1 swap TAB-DIG  CDIGEST-DIGEST:EQ ;

: TABLE-STABLE-CASE ( -- )
   s" the same table digests to the same value twice" T-LABEL
   BND [: TABLE-STABLE-BODY ;] IR-CTX:WITH-CONTEXT TTRUE ;

: TABLE-MOVES-CASE ( -- )
   s" a table with one more schema digests differently" T-LABEL
   BND [: TABLE-MOVES-BODY ;] IR-CTX:WITH-CONTEXT TFALSE ;

: VERIFY-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: tp:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c sp sr tp tr key V-BASE STAGE
   c a r key sr tr IR-SCHEMA:DEFINE
   a r  a r IR-SCHEMA:TABLE-DIGEST  IR-SCHEMA:VERIFY ;

: VERIFY-BAD-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: tp:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c sp sr tp tr key V-BASE STAGE
   c a r key sr tr IR-SCHEMA:DEFINE
   a r  1 2 3 4 CDIGEST-DIGEST:MAKE  IR-SCHEMA:VERIFY ;

: VERIFY-BAD-RUN ( -- )  BND [: VERIFY-BAD-BODY ;] IR-CTX:WITH-CONTEXT ;

\ Every varying field, one fixture each: the digest must move.
: DIG-CASES ( -- )
   STABLE-CASE
   TABLE-STABLE-CASE
   TABLE-MOVES-CASE
   s" the opcode name participates in the digest" T-LABEL       V-NAME DIG-MOVES
   s" an operand type participates in the digest" T-LABEL       V-OPTYPE DIG-MOVES
   s" the operand count participates in the digest" T-LABEL     V-OPN DIG-MOVES
   s" the operand tail flag participates in the digest" T-LABEL V-OPTAIL DIG-MOVES
   s" a result type participates in the digest" T-LABEL         V-RSTYPE DIG-MOVES
   s" the result count participates in the digest" T-LABEL      V-RSN DIG-MOVES
   s" the result tail flag participates in the digest" T-LABEL  V-RSTAIL DIG-MOVES
   s" the region count participates in the digest" T-LABEL      V-REGN DIG-MOVES
   s" a required attribute key participates in the digest" T-LABEL V-ATTR DIG-MOVES
   s" the attribute count participates in the digest" T-LABEL   V-ATN DIG-MOVES
   s" the extension flag participates in the digest" T-LABEL    V-ATEXT DIG-MOVES
   s" the effect class participates in the digest" T-LABEL      V-EFF DIG-MOVES
   s" the effect domain participates in the digest" T-LABEL     V-DOM DIG-MOVES
   s" the memory space participates in the digest" T-LABEL      V-SPC DIG-MOVES
   s" the alias behaviour participates in the digest" T-LABEL   V-ALI DIG-MOVES
   s" the required features participate in the digest" T-LABEL  V-FEAT DIG-MOVES
   s" the semantic rule participates in the digest" T-LABEL     V-RULE DIG-MOVES
   s" the renderer participates in the digest" T-LABEL          V-REND DIG-MOVES
   s" the owning dialect participates in the digest" T-LABEL    V-DIALECT DIG-MOVES
   s" a declared tie participates in the digest" T-LABEL        V-TIE DIG-MOVES
   s" the successor count participates in the digest" T-LABEL   V-SUCC CDIG-MOVES
   s" the terminator flag participates in the digest" T-LABEL   V-TERM CDIG-MOVES
   ARCH-DIG-CASE
   s" a table verifies against its own recomputed digest" T-LABEL
   BND [: VERIFY-BODY ;] IR-CTX:WITH-CONTEXT
   s" a digest that does not match the table rejects" T-LABEL
   [: VERIFY-BAD-RUN ;] E-IR-SCHEMA-DIGEST TTHROWSQ ;

\ ---- trap flag ---------------------------------------------------------------
: TRAP-BODY ( IR-CTX:ctx -- bool )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: tp:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c sp sr key V-BASE OP-SYM IR-SCHEMA:BEGIN-OP
   false 0 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   true IR-SCHEMA:SET-TRAP
   CTARGET-ARCH:AARCH64 CTARGET:F-BASE IR-SCHEMA:SET-TARGET
   c sp sr key V-BASE RULE-SYM IR-SCHEMA:SET-RULE
   c sp sr key V-BASE REND-SYM IR-SCHEMA:SET-RENDERER
   c a r key sr tr IR-SCHEMA:DEFINE
   r  c sp sr key V-BASE OP-SYM  IR-SCHEMA:TRAPS? ;

: TRAP-CASE ( -- )
   s" the may-trap flag reads back as declared" T-LABEL
   BND [: TRAP-BODY ;] IR-CTX:WITH-CONTEXT TTRUE
   s" the trap flag participates in the digest" T-LABEL
   V-TRAP DIG-MOVES ;

\ ---- cross-owner references --------------------------------------------------
: FKEY-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c IR-CTX:NEW-MODULE drop {: other:IR-ID:ir-module-key :}
   r other IR-SCHEMA:DIALECT@ drop ;

: FSYM-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c IR-CTX:NEW-MODULE drop {: other:IR-ID:ir-module-key :}
   c other SYM-NEW {: op2:IR-ARENA:arena or2:IR-ARENA:arena :}
   r  c op2 or2 other s" hir.add" IR-SYM:INTERN  IR-SCHEMA:DEFINED? drop ;

: FTYPE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: tp:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c IR-CTX:NEW-MODULE drop {: other:IR-ID:ir-module-key :}
   c other TYP-NEW {: tp2:IR-ARENA:arena tr2:IR-ARENA:arena :}
   c sp sr key V-BASE OP-SYM IR-SCHEMA:BEGIN-OP
   c tp2 tr2 other I64 IR-SCHEMA:ADD-OPERAND
   false 0 0 IR-SCHEMA:SET-CONTROL
   IR-SCHEMA:SET-PURE
   false IR-SCHEMA:SET-TRAP
   CTARGET-ARCH:AARCH64 CTARGET:F-BASE IR-SCHEMA:SET-TARGET
   c sp sr key V-BASE RULE-SYM IR-SCHEMA:SET-RULE
   c sp sr key V-BASE REND-SYM IR-SCHEMA:SET-RENDERER
   c a r key sr tr IR-SCHEMA:DEFINE ;

: FDIA-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c IR-CTX:NEW-MODULE drop {: other:IR-ID:ir-module-key :}
   c other SYM-NEW {: op2:IR-ARENA:arena or2:IR-ARENA:arena :}
   c sr key  c op2 or2 other s" hir" IR-SYM:INTERN  1 0 8 64 IR-SCHEMA:NEW
   2drop ;

\ A table built under one context, read with a key minted by another.
: FCTX-KEY ( IR-CTX:ctx -- IR-ID:ir-module-key )
   IR-CTX:NEW-MODULE drop ;

: FCTX-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   BND [: FCTX-KEY ;] IR-CTX:WITH-CONTEXT {: other:IR-ID:ir-module-key :}
   r other IR-SCHEMA:DIALECT@ drop ;

: FKEY-RUN ( -- )   BND [: FKEY-BODY ;] IR-CTX:WITH-CONTEXT ;
: FSYM-RUN ( -- )   BND [: FSYM-BODY ;] IR-CTX:WITH-CONTEXT ;
: FTYPE-RUN ( -- )  BND [: FTYPE-BODY ;] IR-CTX:WITH-CONTEXT ;
: FDIA-RUN ( -- )   BND [: FDIA-BODY ;] IR-CTX:WITH-CONTEXT ;
: FCTX-RUN ( -- )   BND [: FCTX-BODY ;] IR-CTX:WITH-CONTEXT ;

: OWNER-CASES ( -- )
   s" a foreign module key rejects at the table" T-LABEL
   [: FKEY-RUN ;] E-IR-SCHEMA-OWNER TTHROWSQ
   s" another module's opcode symbol rejects" T-LABEL
   [: FSYM-RUN ;] E-IR-SCHEMA-OWNER TTHROWSQ
   s" another module's operand type rejects" T-LABEL
   [: FTYPE-RUN ;] E-IR-SCHEMA-OWNER TTHROWSQ
   s" another module's dialect symbol rejects at creation" T-LABEL
   [: FDIA-RUN ;] E-IR-SCHEMA-OWNER TTHROWSQ
   s" a key minted by another context does not open this table" T-LABEL
   [: FCTX-RUN ;] E-IR-SCHEMA-OWNER TTHROWSQ ;

\ ---- capacities --------------------------------------------------------------
: CAP-BODY ( IR-CTX:ctx n n -- )
   {: c:IR-CTX:ctx rcap:n pcap:n :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c sr key  c sp sr key s" hir" IR-SYM:INTERN  1 0 rcap pcap IR-SCHEMA:NEW
   2drop ;

: VER-BODY ( IR-CTX:ctx n n -- )
   {: c:IR-CTX:ctx major:n minor:n :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c sr key  c sp sr key s" hir" IR-SYM:INTERN  major minor 8 64 IR-SCHEMA:NEW
   2drop ;

\ Fill a one-row table, then prove the second definition rejects and the table
\ is still readable.
: FULL-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: tp:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sr key  c sp sr key s" hir" IR-SYM:INTERN  1 0 1 64 IR-SCHEMA:NEW
   {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c sp sr tp tr key V-BASE STAGE
   c a r key sr tr IR-SCHEMA:DEFINE
   c sp sr tp tr key V-NAME STAGE
   c a r key sr tr IR-SCHEMA:DEFINE ;

: POOL-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: tp:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sr key  c sp sr key s" hir" IR-SYM:INTERN  1 0 8 1 IR-SCHEMA:NEW
   {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c sp sr tp tr key V-BASE STAGE
   c a r key sr tr IR-SCHEMA:DEFINE ;

: CAP-ZERO-RUN ( -- )   BND [: 0 64 CAP-BODY ;] IR-CTX:WITH-CONTEXT ;
: CAP-BIG-RUN ( -- )    BND [: 257 64 CAP-BODY ;] IR-CTX:WITH-CONTEXT ;
: CAP-POOL0-RUN ( -- )  BND [: 8 0 CAP-BODY ;] IR-CTX:WITH-CONTEXT ;
: VER-NEG-RUN ( -- )    BND [: -1 0 VER-BODY ;] IR-CTX:WITH-CONTEXT ;
: VER-BIG-RUN ( -- )    BND [: 1 65536 VER-BODY ;] IR-CTX:WITH-CONTEXT ;
: FULL-RUN ( -- )       BND [: FULL-BODY ;] IR-CTX:WITH-CONTEXT ;
: POOL-RUN ( -- )       BND [: POOL-BODY ;] IR-CTX:WITH-CONTEXT ;

: CAP-CASES ( -- )
   s" a zero row capacity rejects" T-LABEL
   [: CAP-ZERO-RUN ;] E-IR-SCHEMA-CAP TTHROWSQ
   s" a row capacity past the committed ceiling rejects" T-LABEL
   [: CAP-BIG-RUN ;] E-IR-SCHEMA-CAP TTHROWSQ
   s" a zero pool capacity rejects" T-LABEL
   [: CAP-POOL0-RUN ;] E-IR-SCHEMA-CAP TTHROWSQ
   s" a negative schema version rejects" T-LABEL
   [: VER-NEG-RUN ;] E-IR-SCHEMA-VERSION TTHROWSQ
   s" a schema version past the ceiling rejects" T-LABEL
   [: VER-BIG-RUN ;] E-IR-SCHEMA-VERSION TTHROWSQ
   s" a definition past the row ceiling rejects" T-LABEL
   [: FULL-RUN ;] E-IR-SCHEMA-CAP TTHROWSQ
   s" a definition past the pool ceiling rejects" T-LABEL
   [: POOL-RUN ;] E-IR-SCHEMA-CAP TTHROWSQ ;

\ ---- list bounds -------------------------------------------------------------
: IDX-BODY ( IR-CTX:ctx n -- )
   {: c:IR-CTX:ctx i:n :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: tp:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c sp sr tp tr key V-BASE STAGE
   c a r key sr tr IR-SCHEMA:DEFINE
   a r key  c sp sr key V-BASE OP-SYM  i IR-SCHEMA:OPERAND@ drop ;

: IDX-PAST-RUN ( -- )  BND [: 1 IDX-BODY ;] IR-CTX:WITH-CONTEXT ;
: IDX-NEG-RUN ( -- )   BND [: -1 IDX-BODY ;] IR-CTX:WITH-CONTEXT ;

: IDX-CASES ( -- )
   s" an operand index past the list rejects" T-LABEL
   [: IDX-PAST-RUN ;] E-IR-SCHEMA-BOUND TTHROWSQ
   s" a negative operand index rejects" T-LABEL
   [: IDX-NEG-RUN ;] E-IR-SCHEMA-BOUND TTHROWSQ ;

\ ---- non-table, misaligned, and forged rows ----------------------------------
: RAW-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 8 IR-ARENA:NEW IR-SCHEMA:SCHEMAS drop ;

\ The two stores are not interchangeable: the pool presented as the row table
\ is a format-tag reject, not a misread.
: SWAP-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   a IR-SCHEMA:SCHEMAS drop ;

: SHAPE2-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c r 7 IR-ARENA:PUSH drop
   r IR-SCHEMA:SCHEMAS drop ;

\ The value one raw forged cell takes: the fields of a minimal valid pure
\ schema, with cell `off` replaced by `val`.
: FVAL ( n n n n -- n )
   {: i:n nm:n off:n val:n :}
   i off = if val exit then
   i F-NAME = if nm exit then
   i F-DOM = if -1 exit then
   i F-SPC = if -1 exit then
   i F-ALI = if -1 exit then
   i F-FEAT = if 1 exit then
   0 ;

\ Append one row past this package's constructors.
: FORGE ( IR-CTX:ctx IR-ARENA:arena n n n -- )
   {: c:IR-CTX:ctx r:IR-ARENA:arena nm:n off:n val:n :}
   ROW-CELLS 0 ?do
      c r  i nm off val FVAL  IR-ARENA:PUSH drop
   loop ;

\ A forged row cannot smuggle a value outside a closed vocabulary past a
\ decoder: the row is appended for a name the module holds but never defined,
\ and the named reader rejects.
: FORGE-BODY ( n n n IR-CTX:ctx -- )
   {: off:n val:n k:n c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c sp sr key s" hir.forged" IR-SYM:INTERN {: op:IR-ID:ir-symbol-id :}
   c r op IR-ID:SYMBOL-LOCAL off val FORGE
   k 1 = if r op IR-SCHEMA:EFFECT@ drop then
   k 2 = if r op IR-SCHEMA:ARCH@ drop then
   k 3 = if r op IR-SCHEMA:TRAPS? drop then
   k 4 = if r op IR-SCHEMA:TERMINATOR? drop then
   k 5 = if r op IR-SCHEMA:OPERAND-TAIL? drop then
   k 6 = if r op IR-SCHEMA:ATTR-EXT? drop then
   k 7 = if r op IR-SCHEMA:DOMAIN@ drop then
   k 8 = if r op IR-SCHEMA:SPACE@ drop then
   k 9 = if r op IR-SCHEMA:ALIAS@ drop then
   k 10 = if r key op IR-SCHEMA:RULE@ drop then
   k 11 = if a r key op 0 IR-SCHEMA:OPERAND@ drop then
   k 12 = if r op IR-SCHEMA:FEATURES@ drop then
   k 13 = if a r op 0 IR-SCHEMA:TIE-RESULT@ drop then ;

: FORGE-RUN ( n n n -- )
   BND [: FORGE-BODY ;] IR-CTX:WITH-CONTEXT ;

: RAW-RUN ( -- )      BND [: RAW-BODY ;] IR-CTX:WITH-CONTEXT ;
: SWAP-RUN ( -- )     BND [: SWAP-BODY ;] IR-CTX:WITH-CONTEXT ;
: SHAPE2-RUN ( -- )   BND [: SHAPE2-BODY ;] IR-CTX:WITH-CONTEXT ;

: STATE-CASES ( -- )
   s" a bare arena is not a schema table" T-LABEL
   [: RAW-RUN ;] E-IR-SCHEMA-STATE TTHROWSQ
   s" the list pool presented as the row table rejects" T-LABEL
   [: SWAP-RUN ;] E-IR-SCHEMA-STATE TTHROWSQ
   s" a misaligned row shape rejects fail-closed" T-LABEL
   [: SHAPE2-RUN ;] E-IR-SCHEMA-STATE TTHROWSQ
   s" a forged effect class outside the vocabulary rejects" T-LABEL
   [: F-EFF 99 1 FORGE-RUN ;] E-IR-SCHEMA-STATE TTHROWSQ
   s" a forged architecture code outside the vocabulary rejects" T-LABEL
   [: F-ARCH 9 2 FORGE-RUN ;] E-IR-SCHEMA-STATE TTHROWSQ
   s" a forged trap cell that is neither truth value rejects" T-LABEL
   [: F-TRAP 7 3 FORGE-RUN ;] E-IR-SCHEMA-STATE TTHROWSQ
   s" a forged terminator cell that is neither truth value rejects" T-LABEL
   [: F-TERM 7 4 FORGE-RUN ;] E-IR-SCHEMA-STATE TTHROWSQ
   s" a forged operand tail cell that is neither truth value rejects" T-LABEL
   [: F-OPTAIL 7 5 FORGE-RUN ;] E-IR-SCHEMA-STATE TTHROWSQ
   s" a forged extension cell that is neither truth value rejects" T-LABEL
   [: F-ATEXT 7 6 FORGE-RUN ;] E-IR-SCHEMA-STATE TTHROWSQ
   s" a forged effect domain outside the vocabulary rejects" T-LABEL
   [: F-EFF 1 7 FORGE-RUN ;] E-IR-SCHEMA-STATE TTHROWSQ
   s" a forged memory space outside the vocabulary rejects" T-LABEL
   [: F-SPC 9 8 FORGE-RUN ;] E-IR-SCHEMA-KIND TTHROWSQ
   s" a forged alias behaviour outside the vocabulary rejects" T-LABEL
   [: F-ALI 9 9 FORGE-RUN ;] E-IR-SCHEMA-KIND TTHROWSQ
   s" a forged negative rule ordinal rejects" T-LABEL
   [: F-RULE -3 10 FORGE-RUN ;] E-IR-SCHEMA-STATE TTHROWSQ
   s" a forged operand window past the pool rejects" T-LABEL
   [: F-OPN 4 11 FORGE-RUN ;] E-IR-SCHEMA-STATE TTHROWSQ
   s" a forged feature word outside the known bits rejects" T-LABEL
   [: F-FEAT $8000 12 FORGE-RUN ;] E-CTGT-FEATURE-BITS TTHROWSQ
   s" a forged tie window past the pool rejects" T-LABEL
   [: F-TIN 4 13 FORGE-RUN ;] E-IR-SCHEMA-STATE TTHROWSQ ;

\ ---- frozen modules ----------------------------------------------------------
: FZ-BODY ( IR-CTX:ctx -- n bool n n bool bool )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: tp:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c sp sr tp tr key V-BASE STAGE
   c a r key sr tr IR-SCHEMA:DEFINE
   c sp sr key V-BASE OP-SYM {: op:IR-ID:ir-symbol-id :}
   a r op IR-SCHEMA:DIGEST CDIGEST-DIGEST:UNMAKE {: w0:n w1:n w2:n w3:n :}
   a r IR-SCHEMA:TABLE-DIGEST CDIGEST-DIGEST:UNMAKE {: t0:n t1:n t2:n t3:n :}
   a IR-ARENA:FREEZE {: pv:IR-ARENA:view :}
   r IR-ARENA:FREEZE {: rv:IR-ARENA:view :}
   rv IR-SCHEMA:FSCHEMAS
   rv op IR-SCHEMA:FDEFINED?
   rv op IR-SCHEMA:FOPERANDS
   rv IR-SCHEMA:FMAJOR@
   pv rv op IR-SCHEMA:FDIGEST
      w0 w1 w2 w3 CDIGEST-DIGEST:MAKE CDIGEST-DIGEST:EQ
   pv rv IR-SCHEMA:FTABLE-DIGEST
      t0 t1 t2 t3 CDIGEST-DIGEST:MAKE CDIGEST-DIGEST:EQ ;

: FZ-CASE ( -- )
   s" a frozen module serves the schema readers through the views" T-LABEL
   BND [: FZ-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE 1 T= 1 T= TTRUE 1 T= ;

: FZ2-BODY ( IR-CTX:ctx -- bool bool bool bool )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: tp:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c sp sr tp tr key V-BASE STAGE
   c a r key sr tr IR-SCHEMA:DEFINE
   c sp sr key V-BASE OP-SYM {: op:IR-ID:ir-symbol-id :}
   a IR-ARENA:FREEZE {: pv:IR-ARENA:view :}
   r IR-ARENA:FREEZE {: rv:IR-ARENA:view :}
   pv rv  pv rv IR-SCHEMA:FTABLE-DIGEST  IR-SCHEMA:FVERIFY
   rv op IR-SCHEMA:FTERMINATOR?
   rv op IR-SCHEMA:FTRAPS?
   rv op IR-SCHEMA:FEFFECT@ IR--SCHEMA-EFFECT:READ IR--SCHEMA-EFFECT:EQ
   rv key op IR-SCHEMA:FRULE@ IR-ID:SYMBOL-LOCAL
      c sp sr key V-BASE RULE-SYM IR-ID:SYMBOL-LOCAL = ;

: FZ2-CASE ( -- )
   s" the frozen table verifies and every frozen field reader answers" T-LABEL
   BND [: FZ2-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TFALSE TFALSE ;

\ The retired builder handles reject every touch once the module is frozen.
: FZ-RETIRED-BODY ( n IR-CTX:ctx -- )
   {: k:n c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c key TYP-NEW {: tp:IR-ARENA:arena tr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   c sp sr tp tr key V-BASE STAGE
   c a r key sr tr IR-SCHEMA:DEFINE
   a IR-ARENA:FREEZE drop
   r IR-ARENA:FREEZE drop
   k 1 = if r IR-SCHEMA:SCHEMAS drop then
   k 2 = if
      c sp sr tp tr key V-NAME STAGE
      c a r key sr tr IR-SCHEMA:DEFINE
   then ;

: FZ-RETIRED-RUN ( n -- )
   BND [: FZ-RETIRED-BODY ;] IR-CTX:WITH-CONTEXT ;

: FZ-REJECT-CASES ( -- )
   s" live readers reject the retired builder handle" T-LABEL
   [: 1 FZ-RETIRED-RUN ;] E-IR-ARENA-FROZEN TTHROWSQ
   s" defining through retired frozen handles rejects" T-LABEL
   [: 2 FZ-RETIRED-RUN ;] E-IR-ARENA-FROZEN TTHROWSQ ;

\ ---- teardown ----------------------------------------------------------------
: TD-ESC-BODY ( IR-CTX:ctx -- IR-ARENA:arena )
   {: c:IR-CTX:ctx :}
   c IR-CTX:NEW-MODULE drop {: key:IR-ID:ir-module-key :}
   c key SYM-NEW {: sp:IR-ARENA:arena sr:IR-ARENA:arena :}
   c sp sr key V-BASE TAB-NEW {: a:IR-ARENA:arena r:IR-ARENA:arena :}
   r ;

: TD-READ ( -- )
   BND [: TD-ESC-BODY ;] IR-CTX:WITH-CONTEXT
   IR-SCHEMA:SCHEMAS drop ;

: TD-STALE-CASE ( -- )
   s" a schema table is dead after its context ends" T-LABEL
   [: TD-READ ;] E-IR-ARENA-STALE TTHROWSQ ;

: TD-FRESH-CASE ( -- )
   s" fresh contexts and tables succeed after teardown" T-LABEL
   4 0 ?do
      BND [: READ-BODY ;] IR-CTX:WITH-CONTEXT
      0 T= 1 T= TFALSE TFALSE 0 T= 0 T= 1 T= TFALSE 1 T= TTRUE 1 T=
   loop ;

\ ---- the checker keeps the API sealed ----------------------------------------
: CHECKER-CASES ( -- )
   \ positive control: a well-typed candidate over the same surface certifies,
   \ so the rejections below fail for their stated reason, not a harness typo
   s" IRS-POS ( IR-ARENA:arena -- n ) IR-SCHEMA:SCHEMAS"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" IRS-RAW-OPCODE ( IR-ARENA:arena n -- n ) IR-SCHEMA:OPERANDS"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRS-CTXLESS ( IR-ARENA:arena IR-ARENA:arena IR-ID:ir-module-key IR-ARENA:arena IR-ARENA:arena -- ) IR-SCHEMA:DEFINE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRS-KEYLESS ( IR-CTX:ctx IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena IR-ARENA:arena -- ) IR-SCHEMA:DEFINE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRS-RAW-EFFECT ( IR-TYPE:space IR-SCHEMA:alias n -- ) IR-SCHEMA:SET-MEMORY"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRS-RAW-CONTROL ( n n n -- ) IR-SCHEMA:SET-CONTROL"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRS-RAW-TARGET ( CTARGET:arch n -- ) IR-SCHEMA:SET-TARGET"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRS-VIEW-AS-ARENA ( IR-ARENA:view IR-ID:ir-symbol-id -- n ) IR-SCHEMA:OPERANDS"
      CHECK-QUIET-CANDIDATE! 0 T= ;

\ ---- run ---------------------------------------------------------------------
: HARNESS-READ ( IR-CTX:ctx -- )
   drop
   READ-CASE
   FIELD-CASE
   HDR-CASE
   TRAP-CASE
   FZ-CASE
   FZ2-CASE ;

: HARNESS-OPCODE ( IR-CTX:ctx -- )
   drop
   OPCODE-CASES
   FIELD-CASES ;

: HARNESS-STAGE ( IR-CTX:ctx -- )
   drop
   STAGE-CASES ;

: HARNESS-ARITY ( IR-CTX:ctx -- )
   drop
   ARITY-CASES ;

: HARNESS-EFFECT ( IR-CTX:ctx -- )
   drop
   EFFECT-CASES
   TERM-CASES ;

: HARNESS-TIE ( IR-CTX:ctx -- )
   drop
   TIE-CASES ;

: HARNESS-TARGET ( IR-CTX:ctx -- )
   drop
   TARGET-CASES ;

: HARNESS-OWNER ( IR-CTX:ctx -- )
   drop
   OWNER-CASES ;

: HARNESS-CAP ( IR-CTX:ctx -- )
   drop
   CAP-CASES
   IDX-CASES ;

: HARNESS-STATE ( IR-CTX:ctx -- )
   drop
   STATE-CASES ;

: HARNESS-FROZEN ( IR-CTX:ctx -- )
   drop
   FZ-REJECT-CASES
   TD-STALE-CASE ;

public

\ Throw-through fixtures run inside an outermost harness context, so a context
\ abandoned by a throw is reclaimed by that harness exit instead of holding its
\ arena registry slots for the rest of the process. The groups are split so no
\ one harness accumulates more leaked slots than the registry holds.
: RUN ( -- )
   T-RESET
   BND [: HARNESS-READ ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-OPCODE ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-STAGE ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-ARITY ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-EFFECT ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-TIE ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-TARGET ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-OWNER ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-CAP ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-STATE ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-FROZEN ;] IR-CTX:WITH-CONTEXT
   TORN-CASE
   DIG-CASES
   TD-FRESH-CASE
   CHECKER-CASES
   T-REPORT ;

;package

IR-SCHEMA-TEST:RUN

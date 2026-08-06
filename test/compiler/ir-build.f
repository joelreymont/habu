\ ir-build.f - checked compiler builder and freeze lifecycle tests.
\
\ Proves the sections 6.2, 6.4 and 6.5 contract of src/compiler/ir/build.f: a
\ new builder is unique, live, empty and tied to the context that created it;
\ the ceiling plan must be declared whole before a builder exists; every append
\ word reaches the table its authority owns, so a module built entirely through
\ the builder reads back through the frozen views afterwards; a freeze refused
\ for an open record publishes nothing at all, leaving the context's module
\ count, its scratch cursor, the table counts and the builder itself exactly as
\ they were, so the caller can finish the record and freeze again; a successful
\ freeze transfers the seventeen tables to the context as read-only views and
\ takes every mutation word away from the old handle; abort releases all
\ provisional storage, shown by building and abandoning far more modules than
\ the arena registry has slots for; and each way of misusing a handle -
\ use-after-freeze, double freeze, mutation through a frozen handle, use of an
\ aborted builder, a builder from another context, a builder whose context has
\ torn down - has its own name. Checker fixtures prove the handle families are
\ sealed and that no mutation can be spelled against a frozen module.

require lib/test.f
require test/checker-assert.f
require src/compiler/ir/build.f

package IR-BUILD-TEST
private

16 constant TSLOTS                   \ pins the builder registry capacity
64 constant TARENA-SLOTS             \ pins the arena registry capacity
17 constant TTABLES                  \ pins the tables one module is made of

\ ---- bindings ----------------------------------------------------------------
\ An AArch64 Darwin contract with the baseline instruction set and plain
\ floating point, the same fixture binding the other IR tests use.
: BND ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

\ ---- the ceiling plan every fixture builds against ---------------------------
\ Test-scale ceilings: small enough that many modules fit one context mapping,
\ large enough for the three-opcode dialect and the one function below.
: PLAN-SMALL ( -- )
   IR-BUILD:PLAN-BEGIN
   16 256 IR-BUILD:PLAN-SYMBOLS
   16 64 IR-BUILD:PLAN-TYPES
   16 64 IR-BUILD:PLAN-ATTRS
   8 IR-BUILD:PLAN-SOURCES
   8 64 IR-BUILD:PLAN-SCHEMAS
   16 16 128 IR-BUILD:PLAN-OPS
   8 8 64 IR-BUILD:PLAN-FUNS ;

: MK ( IR-CTX:ctx -- IR-BUILD:builder )
   PLAN-SMALL s" hir" 1 0 IR-BUILD:NEW-BUILDER ;

\ ---- the dialect this module declares ----------------------------------------
\ Three opcodes: an ordinary value-producing operation, one that also declares
\ an attribute key, and a terminator.
0 constant K-CONST
1 constant K-RET
2 constant K-TAGGED                  \ the one opcode that declares an attribute key

: OPC-SYM ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:n :}
   k K-CONST = if c b s" hir.const" IR-BUILD:INTERN-SYMBOL exit then
   k K-TAGGED = if c b s" hir.tagged" IR-BUILD:INTERN-SYMBOL exit then
   c b s" hir.ret" IR-BUILD:INTERN-SYMBOL ;

\ The attribute key hir.tagged requires. An operation has to answer a key its
\ opcode declares, so the schema and the operation name the same symbol.
: ATT-KEY ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" hir.value" IR-BUILD:INTERN-SYMBOL ;

: I64 ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   IR--TYPE-WIDTH:W64 IR--TYPE-SIGN:SIGNED IR-BUILD:INTERN-INT ;

\ Design line 385's signature type: a code reference over one input and one
\ output, which is what design line 456 calls a callable with an effect.
: SIGT ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b I64 {: ty:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   ty IR-TYPE:FN-PARAM
   ty IR-TYPE:FN-RESULT
   c b IR-BUILD:INTERN-CODE-REF ;

: A-SPAN ( IR-CTX:ctx IR-BUILD:builder -- IR-SOURCE:span )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   b  c b s" build-source" IR-BUILD:ADD-SOURCE  0 4 IR-BUILD:ADD-SPAN ;

: SCH-SHAPE ( IR-CTX:ctx IR-BUILD:builder n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:n :}
   k K-CONST = if
      c b I64 IR-SCHEMA:ADD-RESULT
      false 0 0 IR-SCHEMA:SET-CONTROL
      exit
   then
   k K-TAGGED = if
      c b I64 IR-SCHEMA:ADD-RESULT
      c b ATT-KEY IR-SCHEMA:ADD-ATTR
      false 0 0 IR-SCHEMA:SET-CONTROL
      exit
   then
   true 0 0 IR-SCHEMA:SET-CONTROL ;

: SCH-DEF ( IR-CTX:ctx IR-BUILD:builder n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:n :}
   c b k OPC-SYM IR-SCHEMA:BEGIN-OP
   c b k SCH-SHAPE
   IR-SCHEMA:SET-PURE
   false IR-SCHEMA:SET-TRAP
   CTARGET-ARCH:AARCH64 CTARGET:F-BASE IR-SCHEMA:SET-TARGET
   c b s" rule.hir" IR-BUILD:INTERN-SYMBOL IR-SCHEMA:SET-RULE
   c b s" render.hir" IR-BUILD:INTERN-SYMBOL IR-SCHEMA:SET-RENDERER
   c b IR-BUILD:DEFINE-OP ;

: SCH-ALL ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b K-CONST SCH-DEF
   c b K-TAGGED SCH-DEF
   c b K-RET SCH-DEF ;

\ ---- appending one operation through the builder -----------------------------
: OP+ ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-op-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:n :}
   c b  c b k OPC-SYM  IR-BUILD:BEGIN-OP
   c b  c b A-SPAN  IR-BUILD:SET-OP-SPAN
   k K-CONST = if c b  c b I64  IR-BUILD:ADD-RESULT then
   c b IR-BUILD:END-OP ;

\ ---- one whole function, built only through the builder ----------------------
\ A defined, exported, Habu-convention function whose single block holds one
\ constant and one terminator. This is the module every publication fixture
\ freezes.
: FN-OPEN ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b s" main" IR-BUILD:INTERN-SYMBOL  IR-BUILD:BEGIN-FUN
   c b  c b SIGT  IR-BUILD:SET-SIGNATURE
   c b IR--FUN-LINKAGE:DEFINED IR-BUILD:SET-LINKAGE
   c b IR--FUN-VISIBILITY:EXPORTED IR-BUILD:SET-VISIBILITY
   c b IR--FUN-CONVENTION:HABU IR-BUILD:SET-CONVENTION
   c b  c b A-SPAN  IR-BUILD:SET-FUN-SPAN ;

: BLK-BODY ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b IR-BUILD:BEGIN-BLOCK
   c b  c b I64  IR-BUILD:ADD-BLOCK-ARG drop
   c b  c b A-SPAN  IR-BUILD:SET-BLOCK-SPAN
   c b K-CONST OP+ drop
   c b K-RET OP+ drop
   c b IR-BUILD:END-BLOCK drop ;

: MODULE-BUILD ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-fun-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b SCH-ALL
   c b FN-OPEN
   c b BLK-BODY
   c b IR-BUILD:END-FUN ;

\ ---- an operation that carries a keyed attribute -----------------------------
\ Design line 479: an operation names the key each attribute it carries answers,
\ so the freeze verifier can decide that key against the ones the opcode
\ declares. This second module keeps its own function so the counts the
\ publication fixtures above assert stay untouched.
: ATT-OP+ ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-op-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b K-TAGGED OPC-SYM  IR-BUILD:BEGIN-OP
   c b  c b A-SPAN  IR-BUILD:SET-OP-SPAN
   c b  c b I64  IR-BUILD:ADD-RESULT
   c b  c b ATT-KEY  c b 7 IR-BUILD:INTERN-INT-ATTR  IR-BUILD:ADD-ATTR
   c b IR-BUILD:END-OP ;

: ATT-BLK ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-op-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b IR-BUILD:BEGIN-BLOCK
   c b  c b A-SPAN  IR-BUILD:SET-BLOCK-SPAN
   c b ATT-OP+ {: o:IR-ID:ir-op-id :}
   c b K-RET OP+ drop
   c b IR-BUILD:END-BLOCK drop
   o ;

: ATT-BODY ( IR-CTX:ctx -- n bool bool )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b SCH-ALL
   c b FN-OPEN
   c b ATT-BLK {: o:IR-ID:ir-op-id :}
   c b IR-BUILD:END-FUN drop
   c b ATT-KEY {: k:IR-ID:ir-symbol-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FKEY {: key:IR-ID:ir-module-key :}
   m IR-BUILD:FOP-ROWS o IR-OP:FATTRS
   m IR-BUILD:FOP-POOL m IR-BUILD:FOP-ROWS key o 0 IR-OP:FATTR-KEY@
      IR-ID:SYMBOL-LOCAL k IR-ID:SYMBOL-LOCAL =
   m IR-BUILD:FATTR-ROWS
   m IR-BUILD:FOP-POOL m IR-BUILD:FOP-ROWS key o 0 IR-OP:FATTR@
   IR-ATTR:FINT@ 7 = ;

: ATT-CASE ( -- )
   s" an operation carries its attribute under the key it answers" T-LABEL
   BND [: ATT-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE 1 T= ;

\ ---- creation ----------------------------------------------------------------
: CRT-BODY ( IR-CTX:ctx -- bool n n n n n )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   b IR-BUILD:LIVE?
   b IR-BUILD:SYMBOLS
   b IR-BUILD:TYPES
   b IR-BUILD:OPS
   b IR-BUILD:FUNS
   b IR-BUILD:BLOCKS ;

: CREATE-CASE ( -- )
   s" a new builder is live and holds an empty module" T-LABEL
   BND [: CRT-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 0 T= 0 T= 0 T= 1 T= TTRUE ;

\ The dialect name is interned into the module's own symbol table, and the
\ module identity is minted from the context, so creating a builder costs the
\ context exactly one module.
: CRT-ID-BODY ( IR-CTX:ctx -- n n )
   {: c:IR-CTX:ctx :}
   c IR-CTX:MINTED {: before:n :}
   c MK drop
   before
   c IR-CTX:MINTED ;

: CRT-ID-CASE ( -- )
   s" a builder mints exactly one module identity from its context" T-LABEL
   BND [: CRT-ID-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 0 T= ;

\ Two builders of one context are different modules with different keys.
: CRT-TWO-BODY ( IR-CTX:ctx -- bool bool )
   {: c:IR-CTX:ctx :}
   c MK {: b1:IR-BUILD:builder :}
   c MK {: b2:IR-BUILD:builder :}
   b1 IR-BUILD:SERIAL b2 IR-BUILD:SERIAL =
   b1 IR-BUILD:MODULE@ b2 IR-BUILD:MODULE@ IR-ID:MODULE-SAME? ;

: CRT-TWO-CASE ( -- )
   s" two builders of one context are two distinct modules" T-LABEL
   BND [: CRT-TWO-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TFALSE ;

\ ---- the ceiling plan --------------------------------------------------------
: PL-NONE-BODY ( IR-CTX:ctx -- )
   s" hir" 1 0 IR-BUILD:NEW-BUILDER drop ;

: PL-NONE ( -- )
   BND [: PL-NONE-BODY ;] IR-CTX:WITH-CONTEXT ;

: PL-PARTIAL-BODY ( IR-CTX:ctx -- )
   IR-BUILD:PLAN-BEGIN
   16 256 IR-BUILD:PLAN-SYMBOLS
   s" hir" 1 0 IR-BUILD:NEW-BUILDER drop ;

: PL-PARTIAL ( -- )
   BND [: PL-PARTIAL-BODY ;] IR-CTX:WITH-CONTEXT ;

: PL-TWICE ( -- )
   IR-BUILD:PLAN-BEGIN
   16 256 IR-BUILD:PLAN-SYMBOLS
   16 256 IR-BUILD:PLAN-SYMBOLS ;

: PL-NEG ( -- )
   IR-BUILD:PLAN-BEGIN
   -1 IR-BUILD:PLAN-SOURCES ;

: PL-UNOPENED ( -- )
   PLAN-SMALL
   8 IR-BUILD:PLAN-SOURCES ;

\ A plan is consumed by the builder it created, so the next builder needs a
\ fresh one rather than silently reusing the last module's ceilings.
: PL-CONSUMED-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK drop
   c s" hir" 1 0 IR-BUILD:NEW-BUILDER drop ;

: PL-CONSUMED ( -- )
   BND [: PL-CONSUMED-BODY ;] IR-CTX:WITH-CONTEXT ;

: PLAN-CASES ( -- )
   s" a builder with no staged plan rejects" T-LABEL
   [: PL-NONE ;] E-IR-BUILD-PLAN TTHROWSQ
   s" a builder with a partly declared plan rejects" T-LABEL
   [: PL-PARTIAL ;] E-IR-BUILD-PLAN TTHROWSQ
   s" declaring a plan field twice rejects" T-LABEL
   [: PL-TWICE ;] E-IR-BUILD-PLAN TTHROWSQ
   s" a negative ceiling rejects" T-LABEL
   [: PL-NEG ;] E-IR-BUILD-PLAN TTHROWSQ
   s" setting a field with no plan open rejects" T-LABEL
   [: PL-UNOPENED ;] E-IR-BUILD-PLAN TTHROWSQ
   s" the plan is consumed by the builder it created" T-LABEL
   [: PL-CONSUMED ;] E-IR-BUILD-PLAN TTHROWSQ ;

\ ---- the append words reach the tables their authorities own -----------------
: APP-BODY ( IR-CTX:ctx -- n n n n n n n n )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b MODULE-BUILD drop
   b IR-BUILD:SYMBOLS
   b IR-BUILD:TYPES
   b IR-BUILD:SOURCES
   b IR-BUILD:SCHEMAS
   b IR-BUILD:OPS
   b IR-BUILD:VALUES
   b IR-BUILD:FUNS
   b IR-BUILD:BLOCKS ;

\ Eight symbols: the dialect name, three opcode names, the attribute key the
\ tagged opcode declares, the rule and renderer names, and the function name.
\ Two types: the integer and the code reference. Two values: the block argument
\ and the constant's one result - the terminator produces none. Three schemas,
\ one per opcode, and four sources, one per span the module records.
: APPEND-CASE ( -- )
   s" every append word lands in the table its authority owns" T-LABEL
   BND [: APP-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 1 T= 2 T= 2 T= 3 T= 4 T= 2 T= 8 T= ;

\ ---- the interning words -----------------------------------------------------
\ Interning is by value, so the same bytes twice are one identity and the same
\ type twice is one row: the builder adds no second interner of its own.
: INT-BODY ( IR-CTX:ctx -- bool bool n n )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b s" same" IR-BUILD:INTERN-SYMBOL {: s1:IR-ID:ir-symbol-id :}
   c b s" same" IR-BUILD:INTERN-SYMBOL {: s2:IR-ID:ir-symbol-id :}
   c b I64 drop
   c b I64 drop
   c b 7 IR-BUILD:INTERN-INT-ATTR drop
   c b 7 IR-BUILD:INTERN-INT-ATTR drop
   s1 IR-ID:SYMBOL-LOCAL s2 IR-ID:SYMBOL-LOCAL =
   b IR-BUILD:MODULE-KEY  b IR-BUILD:SYMBOLS IR-ID:COUNT  s1 IR-ID:SYMBOL-CHECK
      IR-ID:SYMBOL-LOCAL s1 IR-ID:SYMBOL-LOCAL =
   b IR-BUILD:TYPES
   b IR-BUILD:ATTRS ;

: INTERN-CASE ( -- )
   s" interning through the builder is still by value" T-LABEL
   BND [: INT-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 1 T= TTRUE TTRUE ;

\ ---- freeze publishes the module ---------------------------------------------
: FZ-BODY ( IR-CTX:ctx -- bool bool n n n n )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b MODULE-BUILD {: f0:IR-ID:ir-fun-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FROZEN?
   b IR-BUILD:LIVE?
   m IR-BUILD:FSYM-ROWS IR-SYM:FSYMBOLS
   m IR-BUILD:FOP-ROWS IR-OP:FOPS
   m IR-BUILD:FFUN-ROWS IR-FUN:FFUNS
   m IR-BUILD:FBLOCK-ROWS IR-FUN:FBLOCKS ;

: FREEZE-CASE ( -- )
   s" a frozen module publishes every table as a read-only view" T-LABEL
   BND [: FZ-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 1 T= 2 T= 8 T= TFALSE TTRUE ;

\ The published views serve the records the builder appended, read back through
\ each table's own frozen reader.
: FZ-READ-BODY ( IR-CTX:ctx -- bool bool bool )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b MODULE-BUILD {: f0:IR-ID:ir-fun-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FFUN-ROWS m IR-BUILD:FKEY f0 IR-FUN:FSYMBOL@ {: sym:IR-ID:ir-symbol-id :}
   m IR-BUILD:FSYM-POOL m IR-BUILD:FSYM-ROWS sym s" main" IR-SYM:FEQ?
   m IR-BUILD:FFUN-ROWS f0 IR-FUN:FBLOCK-COUNT 1 =
   m IR-BUILD:FMODULE m IR-BUILD:FMODULE IR-ID:MODULE-SAME? ;

: FREEZE-READ-CASE ( -- )
   s" the published views read back the records the builder appended" T-LABEL
   BND [: FZ-READ-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE ;

\ ---- every published view reaches its table ----------------------------------
\ Seventeen views are published, and a reader of the wrong one would either fail
\ its header check or answer about another table, so reading one record or one
\ count through each is what proves the publication is wired correctly.
32 constant VW-CAP
create VW-BUF VW-CAP allot

: VW-A-BODY ( IR-CTX:ctx -- bool bool bool n )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b MODULE-BUILD drop
   c b I64 {: ty:IR-ID:ir-type-id :}
   c b SIGT {: sig:IR-ID:ir-type-id :}
   c b K-CONST OPC-SYM {: opc:IR-ID:ir-symbol-id :}
   c b s" note" IR-BUILD:INTERN-TEXT-ATTR {: at:IR-ID:ir-attr-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FKEY {: key:IR-ID:ir-module-key :}
   m IR-BUILD:FSYM-POOL m IR-BUILD:FSYM-ROWS opc s" hir.const" IR-SYM:FEQ?
   m IR-BUILD:FTYPE-POOL m IR-BUILD:FTYPE-ROWS key sig 0 IR-TYPE:FPARAM@
      IR-ID:TYPE-LOCAL ty IR-ID:TYPE-LOCAL =
   m IR-BUILD:FSCHEMA-POOL m IR-BUILD:FSCHEMA-ROWS key opc 0 IR-SCHEMA:FRESULT@
      IR-ID:TYPE-LOCAL ty IR-ID:TYPE-LOCAL =
   m IR-BUILD:FATTR-POOL m IR-BUILD:FATTR-ROWS at VW-BUF VW-CAP IR-ATTR:FTEXT-COPY ;

: VW-A-CASE ( -- )
   s" the pool views serve the records the builder interned" T-LABEL
   BND [: VW-A-BODY ;] IR-CTX:WITH-CONTEXT
   4 T= TTRUE TTRUE TTRUE ;

: VW-B-BODY ( IR-CTX:ctx -- n n n n n n n )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b MODULE-BUILD drop
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FSOURCES IR-SOURCE:FSOURCES
   m IR-BUILD:FOP-POOL IR-OP:FPOOL-CELLS
   m IR-BUILD:FVALUE-ROWS IR-OP:FVALUES
   m IR-BUILD:FOP-ROWS IR-OP:FOPS
   m IR-BUILD:FFUN-POOL IR-FUN:FATTR-CELLS
   m IR-BUILD:FFUN-ROWS IR-FUN:FFUNS
   m IR-BUILD:FBLOCK-ROWS IR-FUN:FBLOCKS ;

: VW-B-CASE ( -- )
   s" every row and pool view answers for its own table" T-LABEL
   BND [: VW-B-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 1 T= 0 T= 2 T= 2 T= 1 T= 4 T= ;

\ ---- a frozen builder has no mutation authority left -------------------------
: FZ-SYM-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b IR-BUILD:FREEZE drop
   c b s" late" IR-BUILD:INTERN-SYMBOL drop ;

: FZ-SYM ( -- )
   BND [: FZ-SYM-BODY ;] IR-CTX:WITH-CONTEXT ;

: FZ-OP-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b SCH-ALL
   c b K-CONST OPC-SYM {: op:IR-ID:ir-symbol-id :}
   c b IR-BUILD:FREEZE drop
   c b op IR-BUILD:BEGIN-OP ;

: FZ-OP ( -- )
   BND [: FZ-OP-BODY ;] IR-CTX:WITH-CONTEXT ;

: FZ-FUN-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b s" main" IR-BUILD:INTERN-SYMBOL {: sym:IR-ID:ir-symbol-id :}
   c b IR-BUILD:FREEZE drop
   c b sym IR-BUILD:BEGIN-FUN ;

: FZ-FUN ( -- )
   BND [: FZ-FUN-BODY ;] IR-CTX:WITH-CONTEXT ;

: FZ-READER-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b IR-BUILD:FREEZE drop
   b IR-BUILD:SYMBOLS drop ;

: FZ-READER ( -- )
   BND [: FZ-READER-BODY ;] IR-CTX:WITH-CONTEXT ;

: FZ-TWICE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b IR-BUILD:FREEZE drop
   c b IR-BUILD:FREEZE drop ;

: FZ-TWICE ( -- )
   BND [: FZ-TWICE-BODY ;] IR-CTX:WITH-CONTEXT ;

: FZ-ABORT-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b IR-BUILD:FREEZE drop
   b IR-BUILD:ABORT ;

: FZ-ABORT ( -- )
   BND [: FZ-ABORT-BODY ;] IR-CTX:WITH-CONTEXT ;

: FROZEN-CASES-A ( -- )
   s" interning through a frozen builder rejects" T-LABEL
   [: FZ-SYM ;] E-IR-BUILD-FROZEN TTHROWSQ
   s" opening an operation on a frozen builder rejects" T-LABEL
   [: FZ-OP ;] E-IR-BUILD-FROZEN TTHROWSQ
   s" opening a function on a frozen builder rejects" T-LABEL
   [: FZ-FUN ;] E-IR-BUILD-FROZEN TTHROWSQ ;

: FROZEN-CASES-B ( -- )
   s" a live-count reader on a frozen builder rejects" T-LABEL
   [: FZ-READER ;] E-IR-BUILD-FROZEN TTHROWSQ
   s" freezing twice rejects" T-LABEL
   [: FZ-TWICE ;] E-IR-BUILD-FROZEN TTHROWSQ
   s" aborting after a freeze rejects" T-LABEL
   [: FZ-ABORT ;] E-IR-BUILD-FROZEN TTHROWSQ ;

\ ---- a refused freeze publishes nothing --------------------------------------
\ The context and the builder stay on the data stack rather than being bound as
\ locals, because CATCH restores the stack depth it saw and not the values a
\ locals frame consumed: the fixture below needs both handles back after the
\ refusal. This is the shape CEIL-THIRD already uses for the same reason.
: RF-FREEZE ( IR-CTX:ctx IR-BUILD:builder -- IR-CTX:ctx IR-BUILD:builder )
   2dup IR-BUILD:FREEZE drop ;

\ An operation left open is design line 544's builder-only placeholder. The
\ refusal is observed against every piece of state a publication could have
\ touched: the context's module count and scratch cursor, the table counts, and
\ the builder's own liveness. The same builder then finishes the record and
\ freezes, which proves the refusal cost the module nothing.
: RF-BODY ( IR-CTX:ctx -- n n n n n bool bool )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b SCH-ALL
   c b FN-OPEN
   c b IR-BUILD:BEGIN-BLOCK
   c b  c b A-SPAN  IR-BUILD:SET-BLOCK-SPAN
   c b  c b K-CONST OPC-SYM  IR-BUILD:BEGIN-OP
   c IR-CTX:MINTED {: mint0:n :}
   c IR-CTX:SCRATCH-USED {: scr0:n :}
   b IR-BUILD:OPS {: ops0:n :}
   b IR-BUILD:SYMBOLS {: sym0:n :}
   c b [: RF-FREEZE ;] catch {: c2:IR-CTX:ctx b2:IR-BUILD:builder rc:n :}
   rc
   c IR-CTX:MINTED mint0 -
   c IR-CTX:SCRATCH-USED scr0 -
   b2 IR-BUILD:OPS ops0 -
   b2 IR-BUILD:SYMBOLS sym0 -
   b2 IR-BUILD:LIVE?
   c b2 IR-BUILD:ABANDON-OP
   c b2 K-RET OP+ drop
   c b2 IR-BUILD:END-BLOCK drop
   c b2 IR-BUILD:END-FUN drop
   c2 b2 IR-BUILD:FREEZE IR-BUILD:FROZEN? ;

: REFUSE-CASE ( -- )
   s" a freeze refused for an open record publishes nothing" T-LABEL
   BND [: RF-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE 0 T= 0 T= 0 T= 0 T= E-IR-BUILD-OPEN T= ;

: RF-BLOCK-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b SCH-ALL
   c b FN-OPEN
   c b IR-BUILD:BEGIN-BLOCK
   c b IR-BUILD:FREEZE drop ;

: RF-BLOCK ( -- )
   BND [: RF-BLOCK-BODY ;] IR-CTX:WITH-CONTEXT ;

: RF-FUN-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b SCH-ALL
   c b FN-OPEN
   c b IR-BUILD:FREEZE drop ;

: RF-FUN ( -- )
   BND [: RF-FUN-BODY ;] IR-CTX:WITH-CONTEXT ;

: REFUSE-BLOCK-CASE ( -- )
   s" a freeze with a block still open rejects" T-LABEL
   [: RF-BLOCK ;] E-IR-BUILD-OPEN TTHROWSQ ;

: REFUSE-FUN-CASE ( -- )
   s" a freeze with a function still open rejects" T-LABEL
   [: RF-FUN ;] E-IR-BUILD-OPEN TTHROWSQ ;

\ ---- stage ownership ---------------------------------------------------------
\ The two stores keep one open record each per process, so the builder owns
\ them: a second builder cannot open a record while the first holds it, and a
\ builder cannot end a record it never began.
: SG-CROSS-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b1:IR-BUILD:builder :}
   c MK {: b2:IR-BUILD:builder :}
   c b1 SCH-ALL
   c b1  c b1 K-CONST OPC-SYM  IR-BUILD:BEGIN-OP
   c b2 SCH-ALL
   c b2  c b2 K-CONST OPC-SYM  IR-BUILD:BEGIN-OP ;

: SG-CROSS ( -- )
   BND [: SG-CROSS-BODY ;] IR-CTX:WITH-CONTEXT ;

: SG-NOBEGIN-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b IR-BUILD:END-OP drop ;

: SG-NOBEGIN ( -- )
   BND [: SG-NOBEGIN-BODY ;] IR-CTX:WITH-CONTEXT ;

: SG-BLOCK-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b IR-BUILD:BEGIN-BLOCK ;

: SG-BLOCK ( -- )
   BND [: SG-BLOCK-BODY ;] IR-CTX:WITH-CONTEXT ;

: STAGE-CROSS-CASE ( -- )
   s" a second builder cannot open a record the first holds" T-LABEL
   [: SG-CROSS ;] E-IR-BUILD-STAGE TTHROWSQ ;

: STAGE-CASES ( -- )
   s" ending an operation that was never begun rejects" T-LABEL
   [: SG-NOBEGIN ;] E-IR-BUILD-STAGE TTHROWSQ
   s" opening a block outside a function rejects" T-LABEL
   [: SG-BLOCK ;] E-IR-BUILD-STAGE TTHROWSQ ;

\ ---- abort releases all provisional storage ----------------------------------
\ Each module is fifteen arenas and the arena registry holds sixty-four slots,
\ so four modules exhaust it. Building and abandoning TSLOTS modules in one
\ context needs TSLOTS times fifteen slots in total: it can only pass if every
\ ABORT gives its fifteen back at once. The last builder is the one past the
\ builder registry's own capacity, which is a named refusal of its own.
: AB-CYCLE ( IR-CTX:ctx -- IR-CTX:ctx )
   dup MK IR-BUILD:ABORT ;

: AB-RELEASE-BODY ( IR-CTX:ctx -- n n )
   {: c:IR-CTX:ctx :}
   TSLOTS 0 ?do
      c AB-CYCLE drop
   loop
   TSLOTS TTABLES *
   c [: AB-CYCLE ;] catch nip ;

: AB-RELEASE-CASE ( -- )
   s" abort releases every arena a builder took, far past the registry" T-LABEL
   BND [: AB-RELEASE-BODY ;] IR-CTX:WITH-CONTEXT
   E-IR-BUILD-SLOTS T= TSLOTS TTABLES * T=
   s" the abandoned arenas far outnumber the arena registry" T-LABEL
   TSLOTS TTABLES * TARENA-SLOTS > TTRUE ;

\ ---- a builder that runs out part-way gives back what it took ----------------
\ Three modules hold fifty-one of the arena registry's sixty-four slots, which
\ leaves less than a fourth module needs, so a fourth runs out part-way through
\ its tables. Everything it took before that used to stay taken: the builder is
\ published last, so nothing existed that could ABORT them, and only the whole
\ context tearing down would have reclaimed them. Measured on the old code, the
\ context could not take ONE more one-cell arena afterwards; the thirteen the
\ failed builder had taken were gone for good.
\
\ THE MEASUREMENT IS THE FREE SLOTS, TAKEN THROUGH THE REAL ENTRY. The count
\ below asks the arena registry for one-cell arenas until it refuses, so what it
\ answers is how many slots the failed NEW-BUILDER left behind. The expected
\ number is the registry's capacity less what the three live modules hold, both
\ of them pinned constants of this file rather than a number somebody wrote
\ down: the whole of what the fourth took has to come back.
variable PART-FREE

: PART-ARENA ( IR-CTX:ctx n -- IR-CTX:ctx n )
   2dup IR-ARENA:NEW drop ;

: PART-COUNT-FREE ( IR-CTX:ctx -- n )
   0 PART-FREE !
   1
   begin
      [: PART-ARENA ;] catch 0=
   while
      PART-FREE @ 1+ PART-FREE !
   repeat
   2drop PART-FREE @ ;

: PART-MK ( IR-CTX:ctx -- IR-CTX:ctx )
   dup MK drop ;

: PART-BODY ( IR-CTX:ctx -- n n )
   {: c:IR-CTX:ctx :}
   c MK drop
   c MK drop
   c MK drop
   c [: PART-MK ;] catch nip
   c PART-COUNT-FREE ;

: PART-CASE ( -- )
   s" three modules leave the registry short of a fourth" T-LABEL
   TARENA-SLOTS 3 TTABLES * - TTABLES < TTRUE
   s" a builder that runs out of arenas gives back every one it took" T-LABEL
   BND [: PART-BODY ;] IR-CTX:WITH-CONTEXT
   TARENA-SLOTS 3 TTABLES * - T=
   E-IR-ARENA-SLOTS T= ;

\ ---- retire releases a PUBLISHED module's storage -----------------------------
\ The other side of the freeze, and the same measurement. ABORT above proves an
\ ABANDONED builder gives its seventeen arenas back; this proves a module that
\ was published and then superseded gives its seventeen back too. Until RETIRE
\ existed the only way a published slot came back was the whole context tearing
\ down, so a chain of passes that each rewrote the last held every intermediate
\ module to the end of the compilation - which is what ran the native chain out
\ of arenas once a routine both combined and spilled.
\
\ Freezing and retiring TSLOTS modules in one context needs TSLOTS times
\ seventeen arenas in total, which is far past the registry, so this can only
\ pass if every RETIRE gives its whole seventeen back at once.
: RT-CYCLE ( IR-CTX:ctx -- IR-CTX:ctx )
   dup {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b IR-BUILD:FREEZE IR-BUILD:RETIRE ;

\ Eight cycles is what this measures, and the number is bounded from both ends
\ rather than chosen: eight times seventeen arenas is well past the arena
\ registry, so the case cannot pass unless the arenas come back, and eight is
\ under the BUILDER registry's own capacity, which a retire does not refill -
\ a retired slot records that it was retired, exactly as an aborted one does.
8 constant RT-CYCLES

: RT-RELEASE-BODY ( IR-CTX:ctx -- n )
   {: c:IR-CTX:ctx :}
   RT-CYCLES 0 ?do
      c RT-CYCLE drop
   loop
   RT-CYCLES TTABLES * ;

: RT-RELEASE-CASE ( -- )
   s" retire releases every arena a published module held, far past the registry"
   T-LABEL
   BND [: RT-RELEASE-BODY ;] IR-CTX:WITH-CONTEXT
   RT-CYCLES TTABLES * T=
   s" the retired arenas far outnumber the arena registry" T-LABEL
   RT-CYCLES TTABLES * TARENA-SLOTS > TTRUE
   s" and the cycles stay inside the builder registry, which retire does not refill"
   T-LABEL
   RT-CYCLES TSLOTS < TTRUE ;

\ And the storage really comes back rather than merely being forgotten: a
\ context that froze and retired eight modules has exactly as many free arena
\ slots left as one that built nothing. This is the peak-pressure claim stated
\ as a number - a chain that retires what it supersedes costs the modules LIVE
\ at once, not the modules ever built.
\
\ THE TWO COUNTS RUN IN SEPARATE CONTEXTS BECAUSE COUNTING IS DESTRUCTIVE:
\ PART-COUNT-FREE takes arenas until the registry refuses, so a context it has
\ measured has nothing left to build with. One context is measured cold and the
\ other after the cycles, and the two numbers are compared outside both.
: RT-FREE-COLD ( IR-CTX:ctx -- n )
   PART-COUNT-FREE ;

: RT-FREE-AFTER ( IR-CTX:ctx -- n )
   {: c:IR-CTX:ctx :}
   RT-CYCLES 0 ?do
      c RT-CYCLE drop
   loop
   c PART-COUNT-FREE ;

: RT-FREE-CASE ( -- )
   s" a context that retired eight modules has the free slots of one that built none"
   T-LABEL
   BND [: RT-FREE-COLD ;] IR-CTX:WITH-CONTEXT
   BND [: RT-FREE-AFTER ;] IR-CTX:WITH-CONTEXT
   T= ;

\ A retired module answers with its OWN name. This is what makes retiring the
\ wrong module a loud failure instead of a plausible answer: the next reader of
\ the handle is refused rather than served stale tables.
: RT-READ-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:RETIRE
   m IR-BUILD:FKEY drop ;

: RT-READ ( -- )
   BND [: RT-READ-BODY ;] IR-CTX:WITH-CONTEXT ;

\ And a view the module handed out BEFORE it was retired is refused too, by the
\ arena's own seal rather than by anything this package restates. The view was
\ legal when it was taken, which is exactly the dangerous case: it is the handle
\ a pass would still be holding if it kept reading a module somebody else gave
\ back.
: RT-VIEW-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FSYM-POOL {: v:IR-ARENA:view :}
   m IR-BUILD:RETIRE
   v IR-ARENA:SIZE drop ;

: RT-VIEW ( -- )
   BND [: RT-VIEW-BODY ;] IR-CTX:WITH-CONTEXT ;

: RT-STALE-CASES ( -- )
   s" a retired module refuses by its own name" T-LABEL
   [: RT-READ ;] E-IR-BUILD-RETIRED TTHROWSQ
   s" and a view it handed out before the retire is stale, not readable" T-LABEL
   [: RT-VIEW ;] E-IR-ARENA-STALE TTHROWSQ ;

\ An aborted builder answers with its own name, not merely as unknown, and the
\ tables it held are gone with it.
: AB-READ-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   b IR-BUILD:ABORT
   b IR-BUILD:SYMBOLS drop ;

: AB-READ ( -- )
   BND [: AB-READ-BODY ;] IR-CTX:WITH-CONTEXT ;

: AB-MUT-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   b IR-BUILD:ABORT
   c b s" late" IR-BUILD:INTERN-SYMBOL drop ;

: AB-MUT ( -- )
   BND [: AB-MUT-BODY ;] IR-CTX:WITH-CONTEXT ;

: AB-FREEZE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   b IR-BUILD:ABORT
   c b IR-BUILD:FREEZE drop ;

: AB-FREEZE ( -- )
   BND [: AB-FREEZE-BODY ;] IR-CTX:WITH-CONTEXT ;

: AB-TWICE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   b IR-BUILD:ABORT
   b IR-BUILD:ABORT ;

: AB-TWICE ( -- )
   BND [: AB-TWICE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ Abandoning with a record still open gives the stage back rather than refusing,
\ so the next builder starts from a clean staging area.
: AB-OPEN-BODY ( IR-CTX:ctx -- bool bool )
   {: c:IR-CTX:ctx :}
   c MK {: b1:IR-BUILD:builder :}
   c b1 SCH-ALL
   c b1 FN-OPEN
   c b1 IR-BUILD:BEGIN-BLOCK
   c b1  c b1 K-CONST OPC-SYM  IR-BUILD:BEGIN-OP
   b1 IR-BUILD:ABORT
   b1 IR-BUILD:LIVE?
   c MK {: b2:IR-BUILD:builder :}
   c b2 SCH-ALL
   c b2 FN-OPEN
   c b2 IR-BUILD:BEGIN-BLOCK
   c b2  c b2 A-SPAN  IR-BUILD:SET-BLOCK-SPAN
   c b2 K-RET OP+ drop
   c b2 IR-BUILD:END-BLOCK drop
   c b2 IR-BUILD:END-FUN drop
   c b2 IR-BUILD:FREEZE IR-BUILD:FROZEN? ;

: ABORT-CASES ( -- )
   s" reading an aborted builder rejects with its own name" T-LABEL
   [: AB-READ ;] E-IR-BUILD-ABORTED TTHROWSQ
   s" appending through an aborted builder rejects" T-LABEL
   [: AB-MUT ;] E-IR-BUILD-ABORTED TTHROWSQ
   s" freezing an aborted builder rejects" T-LABEL
   [: AB-FREEZE ;] E-IR-BUILD-ABORTED TTHROWSQ
   s" aborting twice rejects" T-LABEL
   [: AB-TWICE ;] E-IR-BUILD-ABORTED TTHROWSQ
   s" abandoning an open record leaves the next builder a clean stage" T-LABEL
   BND [: AB-OPEN-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TFALSE ;

\ ---- a builder belongs to exactly one context --------------------------------
: XC-INNER ( IR-BUILD:builder IR-CTX:ctx -- )
   swap s" foreign" IR-BUILD:INTERN-SYMBOL drop ;

: XC-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK
   BND [: XC-INNER ;] IR-CTX:WITH-CONTEXT ;

: XC ( -- )
   BND [: XC-BODY ;] IR-CTX:WITH-CONTEXT ;

\ Publishing is a state change, so it proves the caller owns the compilation the
\ same way an append does. A live builder presented with a live but foreign
\ context is refused by name, and nothing is published.
: XC-FREEZE-INNER ( IR-BUILD:builder IR-CTX:ctx -- )
   swap IR-BUILD:FREEZE drop ;

: XC-FREEZE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b MODULE-BUILD drop
   b
   BND [: XC-FREEZE-INNER ;] IR-CTX:WITH-CONTEXT ;

: XC-FREEZE ( -- )
   BND [: XC-FREEZE-BODY ;] IR-CTX:WITH-CONTEXT ;

: XC-SIBLING-INNER ( IR-CTX:ctx -- IR-BUILD:builder )
   MK ;

: XC-SIBLING-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   BND [: XC-SIBLING-INNER ;] IR-CTX:WITH-CONTEXT drop
   c MK drop ;

: XC-SIBLING ( -- )
   BND [: XC-SIBLING-BODY ;] IR-CTX:WITH-CONTEXT ;

: XCTX-CASES ( -- )
   s" a builder used with a foreign live context rejects" T-LABEL
   [: XC ;] E-IR-BUILD-OWNER TTHROWSQ
   s" freezing with a foreign live context rejects" T-LABEL
   [: XC-FREEZE ;] E-IR-BUILD-OWNER TTHROWSQ ;

\ ---- a builder dies with its context -----------------------------------------
: ESC-BODY ( IR-CTX:ctx -- IR-BUILD:builder )
   MK ;

: DEAD-BUILDER ( -- IR-BUILD:builder )
   BND [: ESC-BODY ;] IR-CTX:WITH-CONTEXT ;

: ST-READ ( -- )
   DEAD-BUILDER IR-BUILD:SYMBOLS drop ;

\ Freezing needs a live context to present, so the dead builder is carried into
\ a fresh one; the builder's own generation is what is stale, and it is refused
\ before the context it was handed is ever compared.
: ST-FREEZE-INNER ( IR-BUILD:builder IR-CTX:ctx -- )
   swap IR-BUILD:FREEZE drop ;

: ST-FREEZE ( -- )
   DEAD-BUILDER
   BND [: ST-FREEZE-INNER ;] IR-CTX:WITH-CONTEXT ;

: ST-ABORT ( -- )
   DEAD-BUILDER IR-BUILD:ABORT ;

: ST-MUT-INNER ( IR-BUILD:builder IR-CTX:ctx -- )
   swap s" late" IR-BUILD:INTERN-SYMBOL drop ;

: ST-MUT ( -- )
   DEAD-BUILDER
   BND [: ST-MUT-INNER ;] IR-CTX:WITH-CONTEXT ;

: ESC-MOD-BODY ( IR-CTX:ctx -- IR-BUILD:module )
   dup MK IR-BUILD:FREEZE ;

: DEAD-MODULE ( -- IR-BUILD:module )
   BND [: ESC-MOD-BODY ;] IR-CTX:WITH-CONTEXT ;

: ST-VIEW ( -- )
   DEAD-MODULE IR-BUILD:FSYM-ROWS drop ;

: STALE-CASES ( -- )
   s" a builder handle is dead after its context ends" T-LABEL
   DEAD-BUILDER IR-BUILD:LIVE? TFALSE
   s" a frozen module is dead after its context ends" T-LABEL
   DEAD-MODULE IR-BUILD:FROZEN? TFALSE
   s" reading a builder after context teardown rejects" T-LABEL
   [: ST-READ ;] E-IR-BUILD-STALE TTHROWSQ
   s" freezing after context teardown rejects" T-LABEL
   [: ST-FREEZE ;] E-IR-BUILD-STALE TTHROWSQ
   s" aborting after context teardown rejects" T-LABEL
   [: ST-ABORT ;] E-IR-BUILD-STALE TTHROWSQ
   s" appending with a fresh live context still rejects the dead builder" T-LABEL
   [: ST-MUT ;] E-IR-BUILD-STALE TTHROWSQ
   s" a published view is dead after its context ends" T-LABEL
   [: ST-VIEW ;] E-IR-BUILD-STALE TTHROWSQ ;

\ ---- committed ceilings ------------------------------------------------------
\ The ceiling refusal belongs to the table's own authority and lands before any
\ cell is written, so the builder stays usable and freezes the prefix that did
\ land.
: CEIL-PLAN ( -- )
   IR-BUILD:PLAN-BEGIN
   2 64 IR-BUILD:PLAN-SYMBOLS
   4 32 IR-BUILD:PLAN-TYPES
   4 32 IR-BUILD:PLAN-ATTRS
   4 IR-BUILD:PLAN-SOURCES
   4 32 IR-BUILD:PLAN-SCHEMAS
   4 4 32 IR-BUILD:PLAN-OPS
   4 4 32 IR-BUILD:PLAN-FUNS ;

: CEIL-THIRD ( IR-CTX:ctx IR-BUILD:builder -- IR-CTX:ctx IR-BUILD:builder )
   2dup s" three" IR-BUILD:INTERN-SYMBOL drop ;

: CEIL-BODY ( IR-CTX:ctx -- n n bool )
   {: c:IR-CTX:ctx :}
   CEIL-PLAN c s" hir" 1 0 IR-BUILD:NEW-BUILDER {: b:IR-BUILD:builder :}
   c b s" two" IR-BUILD:INTERN-SYMBOL drop
   c b [: CEIL-THIRD ;] catch {: c2:IR-CTX:ctx b2:IR-BUILD:builder rc:n :}
   rc
   b2 IR-BUILD:SYMBOLS
   c2 b2 IR-BUILD:FREEZE IR-BUILD:FROZEN? ;

: CEILING-CASE ( -- )
   s" a table at its committed ceiling refuses and stays freezable" T-LABEL
   BND [: CEIL-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 2 T= E-IR-SYM-CAP T= ;

\ ---- the live readers --------------------------------------------------------
\ Facts about a module that is still being built, read back without a handle to
\ the table that holds them: was this symbol interned, is it spelled this way,
\ which dialect and schema version was this module created for. MK creates its
\ module as dialect `hir` at version 1.0, so the three header answers below are
\ the ones NEW-BUILDER was given and nothing else can have written them.
: LR-BODY ( IR-CTX:ctx -- bool bool bool n n )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b s" square" IR-BUILD:INTERN-SYMBOL {: w:IR-ID:ir-symbol-id :}
   c b w IR-BUILD:SYMBOL-CK
   c b w s" square" IR-BUILD:SYMBOL-IS?
   c b w s" squares" IR-BUILD:SYMBOL-IS?
   c b  c b IR-BUILD:DIALECT@  s" hir" IR-BUILD:SYMBOL-IS?
   c b IR-BUILD:SCHEMA-MAJOR@
   c b IR-BUILD:SCHEMA-MINOR@ ;

: LR-CASE ( -- )
   s" the live readers answer for the module being built" T-LABEL
   BND [: LR-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= 1 T= TTRUE TFALSE TTRUE ;

\ Asking about a symbol leaves the interner exactly as it was: a reader that
\ interned the bytes it was comparing would grow the module it was checking.
: LR-QUIET-BODY ( IR-CTX:ctx -- n n )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b s" square" IR-BUILD:INTERN-SYMBOL {: w:IR-ID:ir-symbol-id :}
   b IR-BUILD:SYMBOLS
   c b w s" never-interned-anywhere" IR-BUILD:SYMBOL-IS? drop
   b IR-BUILD:SYMBOLS ;

: LR-QUIET-CASE ( -- )
   s" asking about a spelling interns nothing" T-LABEL
   BND [: LR-QUIET-BODY ;] IR-CTX:WITH-CONTEXT
   2 T= 2 T= ;

\ A span the module's source registry accepts, and one it does not. The bad span
\ is assembled through the open generated constructor, which is the only way to
\ name bytes outside a registered source.
: LR-SPAN-BODY ( IR-CTX:ctx -- n )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b s" build-source" IR-BUILD:ADD-SOURCE {: s0:IR-ID:ir-source-id :}
   c b  b s0 0 4 IR-BUILD:ADD-SPAN  IR-BUILD:SPAN-CK
   b IR-BUILD:SOURCES ;

: LR-SPAN-CASE ( -- )
   s" a span of a registered source passes the live check" T-LABEL
   BND [: LR-SPAN-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= ;

\ What an opcode's schema declares, and which value an appended operation
\ defined - both asked of a module that is still being built. An elaborator needs
\ the first two before it can stage an operation at all, and the third because
\ IR-OP mints an operation's results itself and END-OP hands back the operation
\ rather than the values. The answer is checked against the operation's own row
\ after the freeze: the value the reader named is the one that operation defines.
: LR-OP-BODY ( IR-CTX:ctx -- n n n bool )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b SCH-ALL
   c b FN-OPEN
   c b IR-BUILD:BEGIN-BLOCK
   c b  c b A-SPAN  IR-BUILD:SET-BLOCK-SPAN
   c b K-CONST OP+ {: o:IR-ID:ir-op-id :}
   c b o 0 IR-BUILD:OP-RESULT@ {: val:IR-ID:ir-value-id :}
   c b K-RET OP+ drop
   c b IR-BUILD:END-BLOCK drop
   c b IR-BUILD:END-FUN drop
   c b  c b K-CONST OPC-SYM  IR-BUILD:SCHEMA-OPERANDS
   c b  c b K-CONST OPC-SYM  IR-BUILD:SCHEMA-RESULTS
   c b  c b K-RET OPC-SYM  IR-BUILD:SCHEMA-RESULTS
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FVALUE-ROWS m IR-BUILD:FOP-ROWS m IR-BUILD:FKEY val
   IR-OP:FVALUE-OP@ IR-ID:OP-LOCAL  o IR-ID:OP-LOCAL = ;

: LR-OP-CASE ( -- )
   s" the schema shape and an operation's result read back before the freeze" T-LABEL
   BND [: LR-OP-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 0 T= 1 T= 0 T= ;

: LR-SPAN-BAD-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b s" build-source" IR-BUILD:ADD-SOURCE {: s0:IR-ID:ir-source-id :}
   c b  s0 4 99 IR--SOURCE-SPAN:MAKE  IR-BUILD:SPAN-CK ;

: LR-SPAN-BAD ( -- )
   BND [: LR-SPAN-BAD-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A symbol identity of this module whose ordinal the interner never minted. It
\ passes every ownership check there is and still does not exist.
: LR-GHOST-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b  b IR-BUILD:MODULE-KEY b IR-BUILD:SYMBOLS IR-ID:PACK-SYMBOL
   IR-BUILD:SYMBOL-CK ;

: LR-GHOST ( -- )
   BND [: LR-GHOST-BODY ;] IR-CTX:WITH-CONTEXT ;

: LR-FROZEN-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b IR-BUILD:FREEZE drop
   c b IR-BUILD:DIALECT@ drop ;

: LR-FROZEN ( -- )
   BND [: LR-FROZEN-BODY ;] IR-CTX:WITH-CONTEXT ;

: LR-ABORTED-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   b IR-BUILD:ABORT
   c b IR-BUILD:SCHEMA-MAJOR@ drop ;

: LR-ABORTED ( -- )
   BND [: LR-ABORTED-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A reader is a use of the builder, so it proves the caller owns the compilation
\ exactly as an append does.
: LR-XC-INNER ( IR-BUILD:builder IR-CTX:ctx -- )
   swap IR-BUILD:DIALECT@ drop ;

: LR-XC-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK
   BND [: LR-XC-INNER ;] IR-CTX:WITH-CONTEXT ;

: LR-XC ( -- )
   BND [: LR-XC-BODY ;] IR-CTX:WITH-CONTEXT ;

\ Every reader carries that gate itself rather than borrowing the gate of the
\ reader a caller happens to run next, so each one is refused on its own.
: LR-XC-SYM-INNER ( IR-BUILD:builder IR-ID:ir-symbol-id IR-CTX:ctx -- )
   {: b:IR-BUILD:builder w:IR-ID:ir-symbol-id c2:IR-CTX:ctx :}
   c2 b w IR-BUILD:SYMBOL-CK ;

: LR-XC-SYM-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   b  c b s" square" IR-BUILD:INTERN-SYMBOL
   BND [: LR-XC-SYM-INNER ;] IR-CTX:WITH-CONTEXT ;

: LR-XC-SYM ( -- )
   BND [: LR-XC-SYM-BODY ;] IR-CTX:WITH-CONTEXT ;

: LR-XC-IS-INNER ( IR-BUILD:builder IR-ID:ir-symbol-id IR-CTX:ctx -- )
   {: b:IR-BUILD:builder w:IR-ID:ir-symbol-id c2:IR-CTX:ctx :}
   c2 b w s" square" IR-BUILD:SYMBOL-IS? drop ;

: LR-XC-IS-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   b  c b s" square" IR-BUILD:INTERN-SYMBOL
   BND [: LR-XC-IS-INNER ;] IR-CTX:WITH-CONTEXT ;

: LR-XC-IS ( -- )
   BND [: LR-XC-IS-BODY ;] IR-CTX:WITH-CONTEXT ;

: LR-XC-SPAN-INNER ( IR-BUILD:builder IR-ID:ir-source-id IR-CTX:ctx -- )
   {: b:IR-BUILD:builder s0:IR-ID:ir-source-id c2:IR-CTX:ctx :}
   c2 b  s0 0 4 IR--SOURCE-SPAN:MAKE  IR-BUILD:SPAN-CK ;

: LR-XC-SPAN-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   b  c b s" build-source" IR-BUILD:ADD-SOURCE
   BND [: LR-XC-SPAN-INNER ;] IR-CTX:WITH-CONTEXT ;

: LR-XC-SPAN ( -- )
   BND [: LR-XC-SPAN-BODY ;] IR-CTX:WITH-CONTEXT ;

: LR-XC-VER-INNER ( IR-BUILD:builder IR-CTX:ctx -- )
   swap IR-BUILD:SCHEMA-MAJOR@ drop ;

: LR-XC-VER-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK
   BND [: LR-XC-VER-INNER ;] IR-CTX:WITH-CONTEXT ;

: LR-XC-VER ( -- )
   BND [: LR-XC-VER-BODY ;] IR-CTX:WITH-CONTEXT ;

: LR-XC-SOP-INNER ( IR-BUILD:builder IR-ID:ir-symbol-id IR-CTX:ctx -- )
   {: b:IR-BUILD:builder op:IR-ID:ir-symbol-id c2:IR-CTX:ctx :}
   c2 b op IR-BUILD:SCHEMA-OPERANDS drop ;

: LR-XC-SOP-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b SCH-ALL
   b  c b K-CONST OPC-SYM
   BND [: LR-XC-SOP-INNER ;] IR-CTX:WITH-CONTEXT ;

: LR-XC-SOP ( -- )
   BND [: LR-XC-SOP-BODY ;] IR-CTX:WITH-CONTEXT ;

: LR-XC-SRS-INNER ( IR-BUILD:builder IR-ID:ir-symbol-id IR-CTX:ctx -- )
   {: b:IR-BUILD:builder op:IR-ID:ir-symbol-id c2:IR-CTX:ctx :}
   c2 b op IR-BUILD:SCHEMA-RESULTS drop ;

: LR-XC-SRS-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b SCH-ALL
   b  c b K-CONST OPC-SYM
   BND [: LR-XC-SRS-INNER ;] IR-CTX:WITH-CONTEXT ;

: LR-XC-SRS ( -- )
   BND [: LR-XC-SRS-BODY ;] IR-CTX:WITH-CONTEXT ;

: LR-XC-RES-INNER ( IR-BUILD:builder IR-ID:ir-op-id IR-CTX:ctx -- )
   {: b:IR-BUILD:builder o:IR-ID:ir-op-id c2:IR-CTX:ctx :}
   c2 b o 0 IR-BUILD:OP-RESULT@ drop ;

: LR-XC-RES-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b SCH-ALL
   c b FN-OPEN
   c b IR-BUILD:BEGIN-BLOCK
   c b  c b A-SPAN  IR-BUILD:SET-BLOCK-SPAN
   b  c b K-CONST OP+
   BND [: LR-XC-RES-INNER ;] IR-CTX:WITH-CONTEXT ;

: LR-XC-RES ( -- )
   BND [: LR-XC-RES-BODY ;] IR-CTX:WITH-CONTEXT ;

: LIVE-REFUSE-CASES-A ( -- )
   s" a span outside its source is refused by the live check" T-LABEL
   [: LR-SPAN-BAD ;] E-IR-SRC-SPAN TTHROWSQ
   s" a symbol identity the interner never minted is refused" T-LABEL
   [: LR-GHOST ;] E-IR-SYM-BOUND TTHROWSQ ;

: LIVE-REFUSE-CASES-B ( -- )
   s" a live reader on a frozen builder rejects" T-LABEL
   [: LR-FROZEN ;] E-IR-BUILD-FROZEN TTHROWSQ
   s" a live reader on an aborted builder rejects" T-LABEL
   [: LR-ABORTED ;] E-IR-BUILD-ABORTED TTHROWSQ ;

: LIVE-REFUSE-CASES-C ( -- )
   s" the dialect reader with a foreign live context rejects" T-LABEL
   [: LR-XC ;] E-IR-BUILD-OWNER TTHROWSQ
   s" the schema-version reader with a foreign live context rejects" T-LABEL
   [: LR-XC-VER ;] E-IR-BUILD-OWNER TTHROWSQ ;

: LIVE-REFUSE-CASES-D ( -- )
   s" the symbol reader with a foreign live context rejects" T-LABEL
   [: LR-XC-SYM ;] E-IR-BUILD-OWNER TTHROWSQ
   s" the spelling reader with a foreign live context rejects" T-LABEL
   [: LR-XC-IS ;] E-IR-BUILD-OWNER TTHROWSQ ;

: LIVE-REFUSE-CASES-E ( -- )
   s" the span reader with a foreign live context rejects" T-LABEL
   [: LR-XC-SPAN ;] E-IR-BUILD-OWNER TTHROWSQ ;

: LIVE-REFUSE-CASES-F ( -- )
   s" the schema operand reader with a foreign live context rejects" T-LABEL
   [: LR-XC-SOP ;] E-IR-BUILD-OWNER TTHROWSQ
   s" the schema result reader with a foreign live context rejects" T-LABEL
   [: LR-XC-SRS ;] E-IR-BUILD-OWNER TTHROWSQ ;

: LIVE-REFUSE-CASES-G ( -- )
   s" the operation result reader with a foreign live context rejects" T-LABEL
   [: LR-XC-RES ;] E-IR-BUILD-OWNER TTHROWSQ ;

\ ---- the checker seals the handle families -----------------------------------
: CHECKER-CASES ( -- )
   s" IRB-FORGE ( n -- IR-BUILD:builder )"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRB-ERASE ( IR-BUILD:builder -- n )"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRB-MOD-FORGE ( n -- IR-BUILD:module )"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRB-MOD-ERASE ( IR-BUILD:module -- n )"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRB-MOD-MUT ( IR-CTX:ctx IR-BUILD:module ptr u8 n -- IR-ID:ir-symbol-id ) IR-BUILD:INTERN-SYMBOL"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRB-MOD-FREEZE ( IR-CTX:ctx IR-BUILD:module -- IR-BUILD:module ) IR-BUILD:FREEZE"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRB-BUILDER-VIEW ( IR-BUILD:builder -- IR-ARENA:view ) IR-BUILD:FSYM-ROWS"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRB-DIALECT-CTXLESS ( IR-BUILD:builder -- IR-ID:ir-symbol-id ) IR-BUILD:DIALECT@"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRB-DIALECT-OF-MODULE ( IR-CTX:ctx IR-BUILD:module -- IR-ID:ir-symbol-id ) IR-BUILD:DIALECT@"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" IRB-LIVE-SYM-ROWS ( IR-CTX:ctx IR-BUILD:builder -- IR-ARENA:arena ) IR-BUILD:SYMBOL-CK"
      CHECK-QUIET-CANDIDATE! 0 T= ;

\ ---- run ---------------------------------------------------------------------
\ Throw-through fixtures are grouped into several short-lived harness contexts,
\ the way the other IR tests do it. A context abandoned by a throw keeps its
\ registry slot, and the arenas and builders it owned, until the nearest
\ enclosing live context leaves normally: a single long harness would run the
\ arena registry out, and a builder abandoned mid-record would still look live
\ enough to hold its stage against the fixtures after it. Every fixture that
\ throws with a record still open therefore gets a harness of its own.
: HARNESS-CREATE ( IR-CTX:ctx -- )
   drop
   CREATE-CASE
   CRT-ID-CASE
   CRT-TWO-CASE
   PLAN-CASES ;

: HARNESS-APPEND ( IR-CTX:ctx -- )
   drop
   APPEND-CASE
   INTERN-CASE ;

: HARNESS-FREEZE ( IR-CTX:ctx -- )
   drop
   FREEZE-CASE
   FREEZE-READ-CASE ;

: HARNESS-VIEWS ( IR-CTX:ctx -- )
   drop
   VW-A-CASE
   VW-B-CASE
   ATT-CASE ;

: HARNESS-FROZEN-A ( IR-CTX:ctx -- )
   drop
   FROZEN-CASES-A ;

: HARNESS-FROZEN-B ( IR-CTX:ctx -- )
   drop
   FROZEN-CASES-B ;

: HARNESS-REFUSE ( IR-CTX:ctx -- )
   drop
   REFUSE-CASE ;

: HARNESS-REFUSE-BLOCK ( IR-CTX:ctx -- )
   drop
   REFUSE-BLOCK-CASE ;

: HARNESS-REFUSE-FUN ( IR-CTX:ctx -- )
   drop
   REFUSE-FUN-CASE ;

: HARNESS-STAGE-CROSS ( IR-CTX:ctx -- )
   drop
   STAGE-CROSS-CASE ;

: HARNESS-STAGE ( IR-CTX:ctx -- )
   drop
   STAGE-CASES ;

: HARNESS-ABORT ( IR-CTX:ctx -- )
   drop
   ABORT-CASES ;

: HARNESS-XCTX ( IR-CTX:ctx -- )
   drop
   XCTX-CASES ;

: HARNESS-CEILING ( IR-CTX:ctx -- )
   drop
   CEILING-CASE ;

: HARNESS-LIVE ( IR-CTX:ctx -- )
   drop
   LR-CASE
   LR-QUIET-CASE
   LR-SPAN-CASE ;

: HARNESS-LIVE-OP ( IR-CTX:ctx -- )
   drop
   LR-OP-CASE ;

: HARNESS-LIVE-REFUSE-A ( IR-CTX:ctx -- )
   drop
   LIVE-REFUSE-CASES-A ;

: HARNESS-LIVE-REFUSE-B ( IR-CTX:ctx -- )
   drop
   LIVE-REFUSE-CASES-B ;

: HARNESS-LIVE-REFUSE-C ( IR-CTX:ctx -- )
   drop
   LIVE-REFUSE-CASES-C ;

: HARNESS-LIVE-REFUSE-D ( IR-CTX:ctx -- )
   drop
   LIVE-REFUSE-CASES-D ;

: HARNESS-LIVE-REFUSE-E ( IR-CTX:ctx -- )
   drop
   LIVE-REFUSE-CASES-E ;

: HARNESS-LIVE-REFUSE-F ( IR-CTX:ctx -- )
   drop
   LIVE-REFUSE-CASES-F ;

: HARNESS-LIVE-REFUSE-G ( IR-CTX:ctx -- )
   drop
   LIVE-REFUSE-CASES-G ;

public

: RUN ( -- )
   T-RESET
   BND [: HARNESS-CREATE ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-APPEND ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-FREEZE ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-VIEWS ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-FROZEN-A ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-FROZEN-B ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-REFUSE ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-REFUSE-BLOCK ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-REFUSE-FUN ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-STAGE-CROSS ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-STAGE ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-ABORT ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-XCTX ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-CEILING ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-LIVE ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-LIVE-OP ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-LIVE-REFUSE-A ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-LIVE-REFUSE-B ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-LIVE-REFUSE-C ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-LIVE-REFUSE-D ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-LIVE-REFUSE-E ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-LIVE-REFUSE-F ;] IR-CTX:WITH-CONTEXT
   BND [: HARNESS-LIVE-REFUSE-G ;] IR-CTX:WITH-CONTEXT
   AB-RELEASE-CASE
   PART-CASE
   RT-RELEASE-CASE
   RT-FREE-CASE
   RT-STALE-CASES
   STALE-CASES
   CHECKER-CASES
   T-REPORT ;

;package

IR-BUILD-TEST:RUN

\ ir-verify.f - checked structural freeze verification tests.
\
\ Proves the sections 5.8, 6.5 and 16.1 contract of src/compiler/ir/verify.f.
\ Every fixture below builds a module through the real IR-BUILD API, corrupts
\ exactly one invariant, and freezes it: the freeze must refuse by the name of
\ the invariant that broke, and refuse before anything is published. A last
\ group builds a module with a diamond of blocks and reads the derived
\ predecessor and successor counts back through the published views, which is
\ what proves the tables the verifier derives are the ones later passes get.
\
\ WHAT A HOSTILE FIXTURE CAN REACH. Several section 6.5 rules are already
\ enforced where the record is appended - IR-FUN refuses a block that does not
\ end in one terminator, IR-OP mints its own result values, both validate every
\ span against the source registry - so no checked program can present a module
\ that breaks them. Those verifier arms are defense in depth against a forged or
\ corrupted table rather than against a caller, they have no fixture here, and
\ the dot records each one and why. The fixtures below are the invariants a
\ checked caller really can break, which is exactly the set the operation and
\ function lanes handed to this verifier.

require lib/test.f
require src/compiler/ir/build.f
require src/compiler/ir/verify.f

package IR-VERIFY-TEST
private

\ ---- bindings ----------------------------------------------------------------
\ The same AArch64 Darwin fixture binding the other IR tests use.
: BND ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

: PLAN-SMALL ( -- )
   IR-BUILD:PLAN-BEGIN
   32 512 IR-BUILD:PLAN-SYMBOLS
   16 64 IR-BUILD:PLAN-TYPES
   16 64 IR-BUILD:PLAN-ATTRS
   64 IR-BUILD:PLAN-SOURCES
   16 128 IR-BUILD:PLAN-SCHEMAS
   32 32 256 IR-BUILD:PLAN-OPS
   8 16 64 IR-BUILD:PLAN-FUNS ;

: MK ( IR-CTX:ctx -- IR-BUILD:builder )
   PLAN-SMALL s" hir" 1 0 IR-BUILD:NEW-BUILDER ;

\ ---- types -------------------------------------------------------------------
: I64 ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   IR--TYPE-WIDTH:W64 IR--TYPE-SIGN:SIGNED IR-BUILD:INTERN-INT ;

: I32 ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   IR--TYPE-WIDTH:W32 IR--TYPE-SIGN:SIGNED IR-BUILD:INTERN-INT ;

: SIGT ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b I64 {: ty:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   ty IR-TYPE:FN-PARAM
   ty IR-TYPE:FN-RESULT
   c b IR-BUILD:INTERN-CODE-REF ;

: A-SPAN ( IR-CTX:ctx IR-BUILD:builder -- IR-SOURCE:span )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   b  c b s" verify-source" IR-BUILD:ADD-SOURCE  0 4 IR-BUILD:ADD-SPAN ;

\ ---- the dialect -------------------------------------------------------------
\ Seven opcodes, chosen so each fixture can break one rule and nothing else.
0 constant K-CONST                   \ no operands, one i64 result
1 constant K-USE                     \ one i64 operand, no result
2 constant K-RET                     \ terminator, no successor
3 constant K-BR                      \ terminator, one successor, no operand
4 constant K-BR1                     \ terminator, one successor, one i64 operand
5 constant K-TAGGED                  \ one i64 result and one required attribute key
6 constant K-MEM                     \ a memory effect with no token to carry it

: OPC-NAME ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:n :}
   k K-CONST = if c b s" hir.const" IR-BUILD:INTERN-SYMBOL exit then
   k K-USE = if c b s" hir.use" IR-BUILD:INTERN-SYMBOL exit then
   k K-RET = if c b s" hir.ret" IR-BUILD:INTERN-SYMBOL exit then
   k K-BR = if c b s" hir.br" IR-BUILD:INTERN-SYMBOL exit then
   k K-BR1 = if c b s" hir.br1" IR-BUILD:INTERN-SYMBOL exit then
   k K-TAGGED = if c b s" hir.tagged" IR-BUILD:INTERN-SYMBOL exit then
   c b s" hir.mem" IR-BUILD:INTERN-SYMBOL ;

: ATT-KEY ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" hir.value" IR-BUILD:INTERN-SYMBOL ;

: SCH-VALUE ( IR-CTX:ctx IR-BUILD:builder n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:n :}
   k K-CONST = if c b I64 IR-SCHEMA:ADD-RESULT then
   k K-USE = if c b I64 IR-SCHEMA:ADD-OPERAND then
   k K-BR1 = if c b I64 IR-SCHEMA:ADD-OPERAND then
   k K-TAGGED = if
      c b I64 IR-SCHEMA:ADD-RESULT
      c b ATT-KEY IR-SCHEMA:ADD-ATTR
   then
   k K-MEM = if c b I64 IR-SCHEMA:ADD-RESULT then ;

: SCH-CTRL ( n -- )
   {: k:n :}
   k K-RET = if true 0 0 IR-SCHEMA:SET-CONTROL exit then
   k K-BR = k K-BR1 = or if true 1 0 IR-SCHEMA:SET-CONTROL exit then
   false 0 0 IR-SCHEMA:SET-CONTROL ;

\ hir.mem declares a data-memory effect and no memory-token operand or result to
\ carry it, which is the one effect rule the schema table cannot decide alone.
: SCH-EFFECT ( n -- )
   K-MEM = if
      IR--TYPE-SPACE:GLOBAL IR--SCHEMA-ALIAS:UNRESTRICTED IR--SCHEMA-EFFECT:READ
      IR-SCHEMA:SET-MEMORY
      exit
   then
   IR-SCHEMA:SET-PURE ;

: SCH-DEF ( IR-CTX:ctx IR-BUILD:builder n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:n :}
   c b k OPC-NAME IR-SCHEMA:BEGIN-OP
   c b k SCH-VALUE
   k SCH-CTRL
   k SCH-EFFECT
   false IR-SCHEMA:SET-TRAP
   CTARGET-ARCH:AARCH64 CTARGET:F-BASE IR-SCHEMA:SET-TARGET
   c b s" rule.hir" IR-BUILD:INTERN-SYMBOL IR-SCHEMA:SET-RULE
   c b s" render.hir" IR-BUILD:INTERN-SYMBOL IR-SCHEMA:SET-RENDERER
   c b IR-BUILD:DEFINE-OP ;

\ Every fixture defines the whole dialect, so a fixture differs from a legal
\ module only in what it then asks the builder to build.
: SCH-ALL ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b K-CONST SCH-DEF
   c b K-USE SCH-DEF
   c b K-RET SCH-DEF
   c b K-BR SCH-DEF
   c b K-BR1 SCH-DEF
   c b K-TAGGED SCH-DEF
   c b K-MEM SCH-DEF ;

\ ---- appending operations ----------------------------------------------------
: OP-OPEN ( IR-CTX:ctx IR-BUILD:builder n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:n :}
   c b  c b k OPC-NAME  IR-BUILD:BEGIN-OP
   c b  c b A-SPAN  IR-BUILD:SET-OP-SPAN ;

: CONST+ ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b K-CONST OP-OPEN
   c b  c b I64  IR-BUILD:ADD-RESULT
   c b IR-BUILD:END-OP {: o:IR-ID:ir-op-id :}
   b IR-BUILD:MODULE-KEY  b IR-BUILD:VALUES 1-  IR-ID:PACK-VALUE ;

: RET+ ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b K-RET OP-OPEN
   c b IR-BUILD:END-OP drop ;

: USE+ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ID:ir-value-id :}
   c b K-USE OP-OPEN
   c b v IR-BUILD:ADD-OPERAND
   c b IR-BUILD:END-OP drop ;

: BR+ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-block-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder t:IR-ID:ir-block-id :}
   c b K-BR OP-OPEN
   c b t IR-BUILD:ADD-SUCCESSOR
   c b IR-BUILD:END-OP drop ;

\ A block identity for an ordinal, which is how a branch names a destination
\ that is still being built - and how a fixture names one that never will be.
: BLK-ID ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-block-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder ord:n :}
   b IR-BUILD:MODULE-KEY ord IR-ID:PACK-BLOCK ;

\ ---- one legal function ------------------------------------------------------
: FN-OPEN ( IR-CTX:ctx IR-BUILD:builder ptr u8 n -- ) \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   {: c:IR-CTX:ctx b:IR-BUILD:builder p u:n :}
   c b  c b p u IR-BUILD:INTERN-SYMBOL  IR-BUILD:BEGIN-FUN
   c b  c b SIGT  IR-BUILD:SET-SIGNATURE
   c b IR--FUN-LINKAGE:DEFINED IR-BUILD:SET-LINKAGE
   c b IR--FUN-VISIBILITY:EXPORTED IR-BUILD:SET-VISIBILITY
   c b IR--FUN-CONVENTION:HABU IR-BUILD:SET-CONVENTION
   c b  c b A-SPAN  IR-BUILD:SET-FUN-SPAN ;

: MAIN-OPEN ( IR-CTX:ctx IR-BUILD:builder -- )
   s" main" FN-OPEN ;

: BLK-OPEN ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b IR-BUILD:BEGIN-BLOCK
   c b  c b A-SPAN  IR-BUILD:SET-BLOCK-SPAN ;

\ The whole legal module: one function, one block, one constant, one return.
: LEGAL ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b SCH-ALL
   c b MAIN-OPEN
   c b BLK-OPEN
   c b CONST+ drop
   c b RET+
   c b IR-BUILD:END-BLOCK drop
   c b IR-BUILD:END-FUN drop ;

: LEGAL-BODY ( IR-CTX:ctx -- bool )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b LEGAL
   c b IR-BUILD:FREEZE IR-BUILD:FROZEN? ;

: LEGAL-CASE ( -- )
   s" a well-formed module passes every structural check" T-LABEL
   BND [: LEGAL-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE ;

\ ---- a successor naming a block that does not exist --------------------------
\ IR-OP validates a successor's owning module but not its existence, because a
\ branch to a block still under construction is ordinary SSA construction. The
\ block this one names is never built, so only the freeze verifier can catch it.
: SUCC-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b SCH-ALL
   c b MAIN-OPEN
   c b BLK-OPEN
   c b  c b 7 BLK-ID  BR+
   c b IR-BUILD:END-BLOCK drop
   c b IR-BUILD:END-FUN drop
   c b IR-BUILD:FREEZE drop ;

: SUCC-RUN ( -- )
   BND [: SUCC-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- a successor whose arguments do not match its destination ----------------
\ The destination block takes one argument and the branch hands it none, which
\ is design line 536's count rule.
: SUCCARG-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b SCH-ALL
   c b MAIN-OPEN
   c b BLK-OPEN
   c b  c b 1 BLK-ID  BR+
   c b IR-BUILD:END-BLOCK drop
   c b BLK-OPEN
   c b  c b I64  IR-BUILD:ADD-BLOCK-ARG drop
   c b RET+
   c b IR-BUILD:END-BLOCK drop
   c b IR-BUILD:END-FUN drop
   c b IR-BUILD:FREEZE drop ;

: SUCCARG-RUN ( -- )
   BND [: SUCCARG-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- a successor argument of the wrong type ----------------------------------
\ The counts agree and the types do not: the branch hands an i64 to a block
\ whose one argument is an i32.
: SUCCTYPE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b SCH-ALL
   c b MAIN-OPEN
   c b BLK-OPEN
   c b CONST+ {: v:IR-ID:ir-value-id :}
   c b K-BR1 OP-OPEN
   c b v IR-BUILD:ADD-OPERAND
   c b  c b 1 BLK-ID  IR-BUILD:ADD-SUCCESSOR
   c b IR-BUILD:END-OP drop
   c b IR-BUILD:END-BLOCK drop
   c b BLK-OPEN
   c b  c b I32  IR-BUILD:ADD-BLOCK-ARG drop
   c b RET+
   c b IR-BUILD:END-BLOCK drop
   c b IR-BUILD:END-FUN drop
   c b IR-BUILD:FREEZE drop ;

: SUCCTYPE-RUN ( -- )
   BND [: SUCCTYPE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- a block argument whose block was never built -----------------------------
\ ADD-BLOCK-ARG mints the value row before the block's own row can exist, so a
\ block abandoned after its arguments leaves values naming a block that is not
\ there. Design section 6.2's answer is the builder's ABORT; the verifier's job
\ is to refuse the module if it is frozen instead.
: ARGDEF-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b SCH-ALL
   c b MAIN-OPEN
   c b IR-BUILD:BEGIN-BLOCK
   c b  c b I64  IR-BUILD:ADD-BLOCK-ARG drop
   c b IR-BUILD:ABANDON-BLOCK
   c b BLK-OPEN
   c b RET+
   c b IR-BUILD:END-BLOCK drop
   c b IR-BUILD:END-FUN drop
   c b IR-BUILD:FREEZE drop ;

: ARGDEF-RUN ( -- )
   BND [: ARGDEF-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- an operation outside every block ----------------------------------------
\ The window tiling proves every operation up to the last block's end belongs to
\ exactly one block. An operation appended after the last block lies past that
\ end, and only the total count catches it.
: OPCOVER-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b LEGAL
   c b CONST+ drop
   c b IR-BUILD:FREEZE drop ;

: OPCOVER-RUN ( -- )
   BND [: OPCOVER-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- a block outside every function ------------------------------------------
\ Blocks built for a function that is then abandoned lie past the last
\ function's block window, which again only the total count catches.
: BLKCOVER-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b LEGAL
   c b s" spare" FN-OPEN
   c b BLK-OPEN
   c b RET+
   c b IR-BUILD:END-BLOCK drop
   c b IR-BUILD:ABANDON-FUN
   c b IR-BUILD:FREEZE drop ;

: BLKCOVER-RUN ( -- )
   BND [: BLKCOVER-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- an operand of the wrong type --------------------------------------------
\ hir.use declares an i64 operand and is handed an i32. The i32 comes from a
\ block argument, because a block argument's type is the block's to choose and no
\ schema constrains it, so this module breaks the operand rule and nothing else.
\ IR-OP counts operands against the schema and leaves their types to design line
\ 542 and this verifier.
: OPTYPE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b SCH-ALL
   c b MAIN-OPEN
   c b IR-BUILD:BEGIN-BLOCK
   c b  c b I32  IR-BUILD:ADD-BLOCK-ARG {: v:IR-ID:ir-value-id :}
   c b  c b A-SPAN  IR-BUILD:SET-BLOCK-SPAN
   c b v USE+
   c b RET+
   c b IR-BUILD:END-BLOCK drop
   c b IR-BUILD:END-FUN drop
   c b IR-BUILD:FREEZE drop ;

: OPTYPE-RUN ( -- )
   BND [: OPTYPE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- a result of the wrong type ----------------------------------------------
\ hir.const declares an i64 result and this one is declared an i32.
: RESTYPE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b SCH-ALL
   c b MAIN-OPEN
   c b BLK-OPEN
   c b K-CONST OP-OPEN
   c b  c b I32  IR-BUILD:ADD-RESULT
   c b IR-BUILD:END-OP drop
   c b RET+
   c b IR-BUILD:END-BLOCK drop
   c b IR-BUILD:END-FUN drop
   c b IR-BUILD:FREEZE drop ;

: RESTYPE-RUN ( -- )
   BND [: RESTYPE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- a required attribute key the operation omits ----------------------------
: MISSKEY-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b SCH-ALL
   c b MAIN-OPEN
   c b BLK-OPEN
   c b K-TAGGED OP-OPEN
   c b  c b I64  IR-BUILD:ADD-RESULT
   c b IR-BUILD:END-OP drop
   c b RET+
   c b IR-BUILD:END-BLOCK drop
   c b IR-BUILD:END-FUN drop
   c b IR-BUILD:FREEZE drop ;

: MISSKEY-RUN ( -- )
   BND [: MISSKEY-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- an attribute key the opcode never declared ------------------------------
\ hir.const declares no attribute key and does not open an extension set, so a
\ key on it is design line 484's unknown attribute.
: EXTRAKEY-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b SCH-ALL
   c b MAIN-OPEN
   c b BLK-OPEN
   c b K-CONST OP-OPEN
   c b  c b I64  IR-BUILD:ADD-RESULT
   c b  c b ATT-KEY  c b 7 IR-BUILD:INTERN-INT-ATTR  IR-BUILD:ADD-ATTR
   c b IR-BUILD:END-OP drop
   c b RET+
   c b IR-BUILD:END-BLOCK drop
   c b IR-BUILD:END-FUN drop
   c b IR-BUILD:FREEZE drop ;

: EXTRAKEY-RUN ( -- )
   BND [: EXTRAKEY-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- an effect with nothing to carry it ---------------------------------------
\ hir.mem declares a data-memory read and no memory-token operand or result.
: EFFECT-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b SCH-ALL
   c b MAIN-OPEN
   c b BLK-OPEN
   c b K-MEM OP-OPEN
   c b  c b I64  IR-BUILD:ADD-RESULT
   c b IR-BUILD:END-OP drop
   c b RET+
   c b IR-BUILD:END-BLOCK drop
   c b IR-BUILD:END-FUN drop
   c b IR-BUILD:FREEZE drop ;

: EFFECT-RUN ( -- )
   BND [: EFFECT-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- a use its definition does not dominate ----------------------------------
\ Three blocks: the entry branches straight to the third, the second defines a
\ value, and the third uses it. IR-OP's rule that an operand names an already
\ minted value is satisfied - the value exists by then - but the block that
\ defines it is not on every path to the block that uses it.
: DOM-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b SCH-ALL
   c b MAIN-OPEN
   c b BLK-OPEN
   c b  c b 2 BLK-ID  BR+
   c b IR-BUILD:END-BLOCK drop
   c b BLK-OPEN
   c b CONST+ {: v:IR-ID:ir-value-id :}
   c b  c b 2 BLK-ID  BR+
   c b IR-BUILD:END-BLOCK drop
   c b BLK-OPEN
   c b v USE+
   c b RET+
   c b IR-BUILD:END-BLOCK drop
   c b IR-BUILD:END-FUN drop
   c b IR-BUILD:FREEZE drop ;

: DOM-RUN ( -- )
   BND [: DOM-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- an operand defined in another function ----------------------------------
\ Value ordinals are module-wide, so a second function can name a value the
\ first one defined. Nothing below the whole module can see that it has.
: SCOPE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b SCH-ALL
   c b MAIN-OPEN
   c b BLK-OPEN
   c b CONST+ {: v:IR-ID:ir-value-id :}
   c b RET+
   c b IR-BUILD:END-BLOCK drop
   c b IR-BUILD:END-FUN drop
   c b s" other" FN-OPEN
   c b BLK-OPEN
   c b v USE+
   c b RET+
   c b IR-BUILD:END-BLOCK drop
   c b IR-BUILD:END-FUN drop
   c b IR-BUILD:FREEZE drop ;

: SCOPE-RUN ( -- )
   BND [: SCOPE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- the derived predecessor and successor tables ----------------------------
\ A diamond: the entry branches to two blocks and both branch to a join. The
\ counts the verifier derives are read back through the published views, which
\ is the only way a later pass will ever see them.
: DIAMOND ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b SCH-ALL
   c b MAIN-OPEN
   c b BLK-OPEN
   c b  c b 1 BLK-ID  BR+
   c b IR-BUILD:END-BLOCK drop
   c b BLK-OPEN
   c b  c b 3 BLK-ID  BR+
   c b IR-BUILD:END-BLOCK drop
   c b BLK-OPEN
   c b  c b 3 BLK-ID  BR+
   c b IR-BUILD:END-BLOCK drop
   c b BLK-OPEN
   c b RET+
   c b IR-BUILD:END-BLOCK drop
   c b IR-BUILD:END-FUN drop ;

: EDGE-BODY ( IR-CTX:ctx -- n n n n n n )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b DIAMOND
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FKEY {: key:IR-ID:ir-module-key :}
   m IR-BUILD:FEDGE-ROWS {: r:IR-ARENA:view :}
   r IR-VERIFY:FEDGE-BLOCKS
   r  key 0 IR-ID:PACK-BLOCK  IR-VERIFY:FPRED-COUNT
   r  key 0 IR-ID:PACK-BLOCK  IR-VERIFY:FSUCC-COUNT
   r  key 3 IR-ID:PACK-BLOCK  IR-VERIFY:FPRED-COUNT
   r  key 3 IR-ID:PACK-BLOCK  IR-VERIFY:FSUCC-COUNT
   m IR-BUILD:FEDGE-POOL r key  key 3 IR-ID:PACK-BLOCK  0 IR-VERIFY:FPRED@
   IR-ID:BLOCK-LOCAL ;

\ The entry has no predecessor and one successor; the join has two predecessors
\ and no successor, and its first predecessor is block one.
: EDGE-CASE ( -- )
   s" the derived tables publish every block's predecessors and successors" T-LABEL
   BND [: EDGE-BODY ;] IR-CTX:WITH-CONTEXT
   1 T= 0 T= 2 T= 1 T= 0 T= 4 T= ;

\ ---- the published readers refuse a bad index --------------------------------
: PREDIDX-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b DIAMOND
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   m IR-BUILD:FKEY {: key:IR-ID:ir-module-key :}
   m IR-BUILD:FEDGE-POOL m IR-BUILD:FEDGE-ROWS key
   key 3 IR-ID:PACK-BLOCK 2 IR-VERIFY:FPRED@ drop ;

: PREDIDX-RUN ( -- )
   BND [: PREDIDX-BODY ;] IR-CTX:WITH-CONTEXT ;

\ ---- a refused freeze publishes nothing --------------------------------------
\ The verifier is a refusal arm in front of the arena freezing, so a module it
\ rejects leaves the builder live, its tables exactly as they were, and nothing
\ published. The tables are append-only, so most refusals cannot be repaired in
\ place - a branch already written cannot be unwritten - and the caller's answer
\ is ABORT. What the fixture therefore measures is that the refusal cost the
\ module nothing and left the verifier itself usable: a second builder in the
\ same context builds a legal module and freezes, which is what proves the
\ verifier's own working arrays carry nothing over from the module it rejected.
: RF-FREEZE ( IR-CTX:ctx IR-BUILD:builder -- IR-CTX:ctx IR-BUILD:builder )
   2dup IR-BUILD:FREEZE drop ;

: RF-BODY ( IR-CTX:ctx -- n n n bool bool )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b SCH-ALL
   c b MAIN-OPEN
   c b BLK-OPEN
   c b  c b 7 BLK-ID  BR+
   c b IR-BUILD:END-BLOCK drop
   c b IR-BUILD:END-FUN drop
   c IR-CTX:MINTED {: mint0:n :}
   b IR-BUILD:OPS {: ops0:n :}
   c b [: RF-FREEZE ;] catch {: c2:IR-CTX:ctx b2:IR-BUILD:builder rc:n :}
   rc
   b2 IR-BUILD:OPS ops0 -
   c2 IR-CTX:MINTED mint0 -
   b2 IR-BUILD:LIVE?
   b2 IR-BUILD:ABORT
   c2 MK {: b3:IR-BUILD:builder :}
   c2 b3 LEGAL
   c2 b3 IR-BUILD:FREEZE IR-BUILD:FROZEN? ;

\ The refusal leaves the builder live, adds no operation, and mints no module
\ identity of its own, and the verifier still accepts the next legal module.
: REFUSE-CASE ( -- )
   s" a freeze the verifier refuses publishes nothing" T-LABEL
   BND [: RF-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE 0 T= 0 T= E-IR-VERIFY-SUCC T= ;

\ ---- cases -------------------------------------------------------------------
\ Each throwing fixture builds its module in a context of its own and abandons
\ it by the throw, and an abandoned context keeps its registry slots until the
\ nearest enclosing live context leaves normally. Seventeen arenas per module
\ against the arena registry's slots therefore means every throwing fixture needs
\ an enclosing harness of its own; grouping them would run the registry out
\ partway through the group and report an arena error instead of the invariant.
: SUCC-CASE ( -- )
   s" a successor naming a block this module never built rejects" T-LABEL
   [: SUCC-RUN ;] E-IR-VERIFY-SUCC TTHROWSQ ;

: SUCCARG-CASE ( -- )
   s" a successor handing its destination no arguments rejects" T-LABEL
   [: SUCCARG-RUN ;] E-IR-VERIFY-SUCCARG TTHROWSQ ;

: SUCCTYPE-CASE ( -- )
   s" a successor argument of the wrong type rejects" T-LABEL
   [: SUCCTYPE-RUN ;] E-IR-VERIFY-SUCCARG TTHROWSQ ;

: ARGDEF-CASE ( -- )
   s" a block argument whose block was abandoned rejects" T-LABEL
   [: ARGDEF-RUN ;] E-IR-VERIFY-ARGDEF TTHROWSQ ;

: OPCOVER-CASE ( -- )
   s" an operation appended after the last block rejects" T-LABEL
   [: OPCOVER-RUN ;] E-IR-VERIFY-COVER TTHROWSQ ;

: BLKCOVER-CASE ( -- )
   s" a block left behind by an abandoned function rejects" T-LABEL
   [: BLKCOVER-RUN ;] E-IR-VERIFY-COVER TTHROWSQ ;

: OPTYPE-CASE ( -- )
   s" an operand whose type the schema does not declare rejects" T-LABEL
   [: OPTYPE-RUN ;] E-IR-VERIFY-OPTYPE TTHROWSQ ;

: RESTYPE-CASE ( -- )
   s" a result whose type the schema does not declare rejects" T-LABEL
   [: RESTYPE-RUN ;] E-IR-VERIFY-RESTYPE TTHROWSQ ;

: MISSKEY-CASE ( -- )
   s" a required attribute key the operation omits rejects" T-LABEL
   [: MISSKEY-RUN ;] E-IR-VERIFY-ATTRKEY TTHROWSQ ;

: EXTRAKEY-CASE ( -- )
   s" an attribute key the opcode never declared rejects" T-LABEL
   [: EXTRAKEY-RUN ;] E-IR-VERIFY-ATTRKEY TTHROWSQ ;

: EFFECT-CASE ( -- )
   s" a memory effect with no token to carry it rejects" T-LABEL
   [: EFFECT-RUN ;] E-IR-VERIFY-EFFECT TTHROWSQ ;

: DOM-CASE ( -- )
   s" a use its definition does not dominate rejects" T-LABEL
   [: DOM-RUN ;] E-IR-VERIFY-DOM TTHROWSQ ;

: SCOPE-CASE ( -- )
   s" an operand defined in another function rejects" T-LABEL
   [: SCOPE-RUN ;] E-IR-VERIFY-SCOPE TTHROWSQ ;

: PREDIDX-CASE ( -- )
   s" a predecessor index past the derived count rejects" T-LABEL
   [: PREDIDX-RUN ;] E-IR-VERIFY-BOUND TTHROWSQ ;

public

: RUN ( -- )
   T-RESET
   BND [: drop LEGAL-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop EDGE-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop SUCC-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop SUCCARG-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop SUCCTYPE-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop ARGDEF-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop OPCOVER-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop BLKCOVER-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop OPTYPE-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop RESTYPE-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop MISSKEY-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop EXTRAKEY-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop EFFECT-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop DOM-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop SCOPE-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop PREDIDX-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop REFUSE-CASE ;] IR-CTX:WITH-CONTEXT
   T-REPORT ;

;package

IR-VERIFY-TEST:RUN

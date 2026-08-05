\ ir-encode.f - checked canonical wire frame tests.
\
\ Proves the section 6.6 contract of src/compiler/ir/encode.f: a frozen module's
\ canonical bytes and their SHA-256 depend on what the module means and on what
\ it was built under, and on nothing else - not on the order its tables were
\ interned in, not on which context built it, not on its module serial - while
\ everything the module orders on purpose stays visible in the bytes.
\
\ HOW THE MAIN FIXTURE WORKS. BUILD builds one module through the real IR-BUILD
\ API and takes two flags. The first reverses the insertion order of every
\ interned table - the five types, the nine named symbols, the four attribute
\ values, the two sources - which stays inside the admissible build orders,
\ because IR-TYPE, IR-ATTR and IR-SOURCE each refuse a row whose referents are
\ not interned yet. So the two modules are the same module built along two
\ topological orders of one reference graph, which is the case
\ formal/Common/Interning.v carries as a counterexample to bare permutation. The
\ second flag swaps the first two operations of the block, which is program order
\ rather than numbering. The fixture measures that the reversal really moved the
\ insertion ordinals before requiring the frames to agree, because a fixture that
\ reversed nothing would pass for the wrong reason.
\
\ WHY THIS FILE DEFINES ITS OWN FIXTURE. test/compiler/ir-canon.f builds a larger
\ module along the same two orders, for the same reason, but its fixture words are
\ private to its own test package. What this file has to exercise is the framing,
\ the field width, the version, the counts and the digest, and the encoder copies
\ the canonical stream slot for slot without ever reading inside it - so a richer
\ payload would not reach any encoder decision that this compact one misses. A
\ shared checked module fixture the canonicalizer, the encoder, and the renderer
\ and diff stage could all build against is worth having; it is dotted rather
\ than done here, because extracting it edits a landed test of another leaf.
\
\ WHAT EACH FIXTURE PROVES. Two topological build orders give one frame and one
\ digest. Two separate contexts give one digest, which is the design's "two
\ independently built equivalent modules have the same digest". Two different
\ numeric policies give two digests, which is the other half: a digest is only
\ meaningful together with what the module was built under. Swapping two
\ operations changes the bytes and the digest. The header readers answer exactly
\ what was encoded, the payload slots are the canonical stream, and the frame is
\ its header plus eight bytes per canonical slot, which pins the field width. The
\ rest are the refusals, one fixture per named error a caller can reach.

require lib/test.f
require src/compiler/ir/build.f
require src/compiler/ir/canon.f
require src/compiler/ir/encode.f

package IR-ENCODE-TEST
private

\ ---- bindings ----------------------------------------------------------------
\ The same AArch64 Darwin fixture binding the other IR tests use, and one that
\ differs in exactly one numeric field.
: BND ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:TRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

: BND-WRAP ( -- CBIND:binding )
   CTARGET-ARCH:AARCH64 CTARGET-ABI:AAPCS64-DARWIN CTARGET-ENDIAN:LITTLE
   CTARGET-PTR--WIDTH:BITS64
   CTARGET:F-BASE CTARGET:F-FP CTARGET:WITH CTARGET:CONTRACT
   CNUM-OVERFLOW:WRAP CNUM-FLOAT--MODEL:IEEE754 CNUM-CONTRACTION:FORBIDDEN
   CNUM-FAST--MATH:BIT-EXACT CNUM-COMPARE:IEEE754-UNORDERED CNUM:POLICY
   CBIND:BIND ;

: PLAN-SMALL ( -- )
   IR-BUILD:PLAN-BEGIN
   32 1024 IR-BUILD:PLAN-SYMBOLS
   16 64 IR-BUILD:PLAN-TYPES
   16 128 IR-BUILD:PLAN-ATTRS
   64 IR-BUILD:PLAN-SOURCES
   16 128 IR-BUILD:PLAN-SCHEMAS
   32 32 256 IR-BUILD:PLAN-OPS
   8 16 64 IR-BUILD:PLAN-FUNS ;

: MK ( IR-CTX:ctx -- IR-BUILD:builder )
   PLAN-SMALL s" hir" 1 0 IR-BUILD:NEW-BUILDER ;

\ ---- types -------------------------------------------------------------------
: I8 ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   IR--TYPE-WIDTH:W8 IR--TYPE-SIGN:SIGNED IR-BUILD:INTERN-INT ;

: I16 ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   IR--TYPE-WIDTH:W16 IR--TYPE-SIGN:SIGNED IR-BUILD:INTERN-INT ;

: I64 ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   IR--TYPE-WIDTH:W64 IR--TYPE-SIGN:SIGNED IR-BUILD:INTERN-INT ;

\ A pointer to i8: the row whose stored pointee ordinal the two orders disagree
\ about.
: PTR8 ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  IR--TYPE-SPACE:GENERIC  c b I8  IR-BUILD:INTERN-POINTER ;

\ A code reference (i64 -- i64): the function's signature, and a row that names a
\ window of type ordinals.
: SIGT ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b I64 {: ty:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   ty IR-TYPE:FN-PARAM
   ty IR-TYPE:FN-RESULT
   c b IR-BUILD:INTERN-CODE-REF ;

\ ---- symbols -----------------------------------------------------------------
\ The two tags are lexicographically first and last of the named symbols, so
\ their canonical order is the reverse of one insertion order.
: A-TAG ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" a-tag" IR-BUILD:INTERN-SYMBOL ;

: Z-TAG ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" z-tag" IR-BUILD:INTERN-SYMBOL ;

: MAIN-SYM ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" main" IR-BUILD:INTERN-SYMBOL ;

: RULE-SYM ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" rule.hir" IR-BUILD:INTERN-SYMBOL ;

: RENDER-SYM ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" render.hir" IR-BUILD:INTERN-SYMBOL ;

\ ---- which order this module is being built along ----------------------------
\ The spans and the operation attribute order both need it, and threading it
\ through every operation helper would say nothing extra, so one fixture-owned
\ cell holds it for the length of one build.
variable REV-CELL

\ ---- the dialect -------------------------------------------------------------
\ Four opcodes: a constant, a tagged constant carrying both attribute keys, a
\ use, and a return.
0 constant K-CONST
1 constant K-TAGGED
2 constant K-USE
3 constant K-RET
4 constant K#

: OPC-NAME ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:n :}
   k K-CONST = if c b s" hir.const" IR-BUILD:INTERN-SYMBOL exit then
   k K-TAGGED = if c b s" hir.tagged" IR-BUILD:INTERN-SYMBOL exit then
   k K-USE = if c b s" hir.use" IR-BUILD:INTERN-SYMBOL exit then
   c b s" hir.ret" IR-BUILD:INTERN-SYMBOL ;

: SCH-VALUE ( IR-CTX:ctx IR-BUILD:builder n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:n :}
   k K-CONST = if c b I64 IR-SCHEMA:ADD-RESULT then
   k K-USE = if c b I64 IR-SCHEMA:ADD-OPERAND then
   k K-TAGGED = if
      c b I64 IR-SCHEMA:ADD-RESULT
      c b Z-TAG IR-SCHEMA:ADD-ATTR
      c b A-TAG IR-SCHEMA:ADD-ATTR
   then ;

: SCH-CTRL ( n -- )
   {: k:n :}
   k K-RET = if true 0 0 IR-SCHEMA:SET-CONTROL exit then
   false 0 0 IR-SCHEMA:SET-CONTROL ;

: SCH-DEF ( IR-CTX:ctx IR-BUILD:builder n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:n :}
   c b k OPC-NAME IR-SCHEMA:BEGIN-OP
   c b k SCH-VALUE
   k SCH-CTRL
   IR-SCHEMA:SET-PURE
   false IR-SCHEMA:SET-TRAP
   CTARGET-ARCH:AARCH64 CTARGET:F-BASE IR-SCHEMA:SET-TARGET
   c b RULE-SYM IR-SCHEMA:SET-RULE
   c b RENDER-SYM IR-SCHEMA:SET-RENDERER
   c b IR-BUILD:DEFINE-OP ;

: SCH-ALL ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   K# 0 ?do
      c b i SCH-DEF
   loop ;

\ ---- the interned tables, in either admissible order -------------------------
\ Each group is one list walked forwards or backwards. Walking backwards is still
\ admissible, because a member that references another interns that other one on
\ the way in: interning the code reference first is what interns i64 first, and
\ interning the record first is what interns its integer values first.
9 constant SYM#

: NTH-SYM ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder n:n :}
   n 0 = if c b MAIN-SYM exit then
   n 1 = if c b A-TAG exit then
   n 2 = if c b Z-TAG exit then
   n 7 = if c b RULE-SYM exit then
   n 8 = if c b RENDER-SYM exit then
   c b n 3 - OPC-NAME ;

: SYMS-IN ( IR-CTX:ctx IR-BUILD:builder n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder rev:n :}
   SYM# 0 ?do
      rev 0= if
         c b i NTH-SYM drop
      else
         c b SYM# 1- i - NTH-SYM drop
      then
   loop ;

: TYPES-IN ( IR-CTX:ctx IR-BUILD:builder n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder rev:n :}
   rev 0= if
      c b I8 drop
      c b I16 drop
      c b I64 drop
      c b PTR8 drop
      c b SIGT drop
      exit
   then
   c b SIGT drop
   c b PTR8 drop
   c b I64 drop
   c b I16 drop
   c b I8 drop ;

: INT7 ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-attr-id )
   7 IR-BUILD:INTERN-INT-ATTR ;

: INT-3 ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-attr-id )
   -3 IR-BUILD:INTERN-INT-ATTR ;

: TXT-A ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-attr-id )
   s" alpha" IR-BUILD:INTERN-TEXT-ATTR ;

\ A record whose two keys are the two tags: its stored pairs are sorted by the
\ key's insertion ordinal, so the two modules store them in opposite orders.
: REC-A ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   IR-ATTR:REC-BEGIN
   c b Z-TAG  c b INT7  IR-ATTR:REC-PAIR
   c b A-TAG  c b INT-3  IR-ATTR:REC-PAIR
   c b IR-BUILD:INTERN-RECORD-ATTR ;

: ATTRS-IN ( IR-CTX:ctx IR-BUILD:builder n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder rev:n :}
   rev 0= if
      c b INT7 drop
      c b INT-3 drop
      c b TXT-A drop
      c b REC-A drop
      exit
   then
   c b REC-A drop
   c b TXT-A drop
   c b INT-3 drop
   c b INT7 drop ;

\ ---- sources and spans -------------------------------------------------------
\ The source registry does not deduplicate, so these two are registered once by
\ the order group and a span names the row that group gave them - a different row
\ in the two builds.
: SRC-ONE+ ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-source-id )
   s" source-one" IR-BUILD:ADD-SOURCE ;

: SRC-TWO+ ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-source-id )
   s" source-two-longer" IR-BUILD:ADD-SOURCE ;

: SRCS-IN ( IR-CTX:ctx IR-BUILD:builder n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder rev:n :}
   rev 0= if
      c b SRC-ONE+ drop
      c b SRC-TWO+ drop
      exit
   then
   c b SRC-TWO+ drop
   c b SRC-ONE+ drop ;

: SRC-ONE ( IR-BUILD:builder -- IR-ID:ir-source-id )
   {: b:IR-BUILD:builder :}
   b IR-BUILD:MODULE-KEY  REV-CELL @ 0= if 0 else 1 then  IR-ID:PACK-SOURCE ;

: SRC-TWO ( IR-BUILD:builder -- IR-ID:ir-source-id )
   {: b:IR-BUILD:builder :}
   b IR-BUILD:MODULE-KEY  REV-CELL @ 0= if 1 else 0 then  IR-ID:PACK-SOURCE ;

: SPAN-ONE ( IR-BUILD:builder -- IR-SOURCE:span )
   {: b:IR-BUILD:builder :}
   b  b SRC-ONE  0 4 IR-BUILD:ADD-SPAN ;

: SPAN-TWO ( IR-BUILD:builder -- IR-SOURCE:span )
   {: b:IR-BUILD:builder :}
   b  b SRC-TWO  2 5 IR-BUILD:ADD-SPAN ;

\ ---- appending operations ----------------------------------------------------
: OP-OPEN ( IR-CTX:ctx IR-BUILD:builder n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:n :}
   c b  c b k OPC-NAME  IR-BUILD:BEGIN-OP
   c b  b SPAN-ONE  IR-BUILD:SET-OP-SPAN ;

: LAST-VALUE ( IR-BUILD:builder -- IR-ID:ir-value-id )
   {: b:IR-BUILD:builder :}
   b IR-BUILD:MODULE-KEY  b IR-BUILD:VALUES 1-  IR-ID:PACK-VALUE ;

: CONST+ ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-value-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b K-CONST OP-OPEN
   c b  c b I64  IR-BUILD:ADD-RESULT
   c b IR-BUILD:END-OP drop
   b LAST-VALUE ;

\ Both declared keys, added in one order or the other. An operation's attributes
\ are keyed, so the order they were added in is no part of the operation and the
\ canonical stream states them sorted by canonical key either way.
: TAG-ATTRS ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   REV-CELL @ 0= if
      c b  c b Z-TAG  c b INT7  IR-BUILD:ADD-ATTR
      c b  c b A-TAG  c b INT-3  IR-BUILD:ADD-ATTR
      exit
   then
   c b  c b A-TAG  c b INT-3  IR-BUILD:ADD-ATTR
   c b  c b Z-TAG  c b INT7  IR-BUILD:ADD-ATTR ;

: TAGGED+ ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b K-TAGGED OP-OPEN
   c b  c b I64  IR-BUILD:ADD-RESULT
   c b TAG-ATTRS
   c b IR-BUILD:END-OP drop ;

: USE+ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ID:ir-value-id :}
   c b K-USE OP-OPEN
   c b v IR-BUILD:ADD-OPERAND
   c b IR-BUILD:END-OP drop ;

: RET+ ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b K-RET OP-OPEN
   c b IR-BUILD:END-OP drop ;

\ ---- the function ------------------------------------------------------------
: FN-OPEN ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b MAIN-SYM  IR-BUILD:BEGIN-FUN
   c b  c b SIGT  IR-BUILD:SET-SIGNATURE
   c b IR--FUN-LINKAGE:DEFINED IR-BUILD:SET-LINKAGE
   c b IR--FUN-VISIBILITY:EXPORTED IR-BUILD:SET-VISIBILITY
   c b IR--FUN-CONVENTION:HABU IR-BUILD:SET-CONVENTION
   c b  c b REC-A  IR-BUILD:ADD-FUN-ATTR
   c b  c b TXT-A  IR-BUILD:ADD-FUN-ATTR
   c b  b SPAN-TWO  IR-BUILD:SET-FUN-SPAN ;

\ The block defines a constant and a tagged constant, uses the constant, and
\ returns. swapped picks which of the two constants is defined first, which is
\ program order and not numbering.
: BLOCK-OPS ( IR-CTX:ctx IR-BUILD:builder n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder swapped:n :}
   swapped 0= if
      c b CONST+ {: v:IR-ID:ir-value-id :}
      c b TAGGED+
      c b v USE+
      c b RET+
      exit
   then
   c b TAGGED+
   c b CONST+ {: v:IR-ID:ir-value-id :}
   c b v USE+
   c b RET+ ;

: BUILD ( IR-CTX:ctx IR-BUILD:builder n n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder rev:n swapped:n :}
   rev REV-CELL !
   c b rev SYMS-IN
   c b rev TYPES-IN
   c b rev ATTRS-IN
   c b rev SRCS-IN
   c b SCH-ALL
   c b FN-OPEN
   c b IR-BUILD:BEGIN-BLOCK
   c b  b SPAN-ONE  IR-BUILD:SET-BLOCK-SPAN
   c b swapped BLOCK-OPS
   c b IR-BUILD:END-BLOCK drop
   c b IR-BUILD:END-FUN drop ;

: MODULE-OF ( IR-CTX:ctx n n -- IR-BUILD:module )
   {: c:IR-CTX:ctx rev:n swapped:n :}
   c MK {: b:IR-BUILD:builder :}
   c b rev swapped BUILD
   c b IR-BUILD:FREEZE ;

\ ---- one frame in the context's scratch ---------------------------------------
\ The destination is a byte span the context owns, sized from SIZE, which is how
\ a production caller sizes a cache record or a witness payload.
: FRAME-OF ( IR-CTX:ctx IR-BUILD:module IR-CANON:table -- ptr u8 n )
   {: c:IR-CTX:ctx m:IR-BUILD:module t:IR-CANON:table :}
   t IR-ENCODE:SIZE {: len:n :}
   c len IR-CTX:SCRATCH-TAKE {: p room:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   c m t p room IR-ENCODE:ENCODE {: wrote:n :}
   p wrote ;

: TABLE-OF ( IR-CTX:ctx n n -- IR-BUILD:module IR-CANON:table )
   {: c:IR-CTX:ctx rev:n swapped:n :}
   c rev swapped MODULE-OF {: m:IR-BUILD:module :}
   m  c m IR-CANON:CANON ;

: ENC-OF ( IR-CTX:ctx n n -- ptr u8 n )
   {: c:IR-CTX:ctx rev:n swapped:n :}
   c rev swapped TABLE-OF {: m:IR-BUILD:module t:IR-CANON:table :}
   c m t FRAME-OF ;

: DIG-OF ( IR-CTX:ctx n n -- CDIGEST:digest )
   ENC-OF IR-ENCODE:DIGEST ;

\ ---- comparing two frames ----------------------------------------------------
: SAME-BYTES? ( ptr u8 n ptr u8 n -- bool )
   {: p pu:n q qu:n :} \ typed-local-lint: allow-bare-local - p and q keep the ptr u8 byte-span role
   pu qu <> if false exit then
   pu 0 ?do
      p i + c@  q i + c@  <> if false unloop exit then
   loop
   true ;

\ ---- two topological build orders give one frame -----------------------------
\ The first two answers measure that the reversal really moved the insertion
\ ordinals; the rest are the contract.
: EQUIV-BODY ( IR-CTX:ctx -- n n n bool bool bool )
   {: c:IR-CTX:ctx :}
   c MK {: ba:IR-BUILD:builder :}
   c ba 0 0 BUILD
   c ba I8 IR-ID:TYPE-LOCAL {: i8a:n :}
   c ba A-TAG IR-ID:SYMBOL-LOCAL {: taga:n :}
   c ba IR-BUILD:FREEZE {: ma:IR-BUILD:module :}
   c MK {: bb:IR-BUILD:builder :}
   c bb 1 0 BUILD
   c bb I8 IR-ID:TYPE-LOCAL {: i8b:n :}
   c bb A-TAG IR-ID:SYMBOL-LOCAL {: tagb:n :}
   c bb IR-BUILD:FREEZE {: mb:IR-BUILD:module :}
   c ma  c ma IR-CANON:CANON  FRAME-OF {: pa pau:n :} \ typed-local-lint: allow-bare-local - pa keeps the ptr u8 byte-span role
   c mb  c mb IR-CANON:CANON  FRAME-OF {: pb pbu:n :} \ typed-local-lint: allow-bare-local - pb keeps the ptr u8 byte-span role
   i8a i8b -
   taga tagb -
   pau pbu -
   pau 0 >
   pa pau pb pbu SAME-BYTES?
   pa pau IR-ENCODE:DIGEST  pb pbu IR-ENCODE:DIGEST  CDIGEST-DIGEST:EQ ;

: EQUIV-CASE ( -- )
   s" two topological build orders encode and digest identically" T-LABEL
   BND [: EQUIV-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE TTRUE 0 T= 0 T<> 0 T<> ;

\ ---- why the frame binds the dialect's version and not its schema digest ------
\ IR-SCHEMA's schema-table digest folds each record's stored operand, result and
\ attribute-key lists, and those hold module-local INSERTION ordinals, so two
\ equivalent modules built along two intern orders have two different
\ schema-table digests. A canonical frame therefore cannot carry it: doing so
\ would make the module identity depend on the build order, which is exactly what
\ canonicalization exists to remove. The frame binds the dialect's canonical name
\ ordinal and its schema major and minor version instead, which is what design
\ section 6.6 asks the header to state. This fixture pins the reason, so a later
\ change that adds the schema digest back into the frame turns it red rather than
\ quietly making equivalent modules differ. The canonical schema digest that
\ could be bound is a missing capability, tracked by its own dot.
: SCHEMA-DIG-BODY ( IR-CTX:ctx -- bool )
   {: c:IR-CTX:ctx :}
   c MK {: ba:IR-BUILD:builder :}
   c ba 0 0 BUILD
   c ba IR-BUILD:FREEZE {: ma:IR-BUILD:module :}
   c MK {: bb:IR-BUILD:builder :}
   c bb 1 0 BUILD
   c bb IR-BUILD:FREEZE {: mb:IR-BUILD:module :}
   ma IR-BUILD:FSCHEMA-POOL ma IR-BUILD:FSCHEMA-ROWS IR-SCHEMA:FTABLE-DIGEST
   mb IR-BUILD:FSCHEMA-POOL mb IR-BUILD:FSCHEMA-ROWS IR-SCHEMA:FTABLE-DIGEST
   CDIGEST-DIGEST:EQ ;

: SCHEMA-DIG-CASE ( -- )
   s" the schema-table digest is not insertion-order independent" T-LABEL
   BND [: SCHEMA-DIG-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE ;

\ ---- two independent contexts give one digest ---------------------------------
\ The frame states no module serial and no context serial, so a module built twice
\ from the same source facts has one identity. This is the design's "two
\ independently built equivalent modules have the same digest".
: CROSS-BODY ( IR-CTX:ctx -- CDIGEST:digest )
   0 0 DIG-OF ;

: CROSS-CASE ( -- )
   s" the same module built in two contexts has one digest" T-LABEL
   BND [: CROSS-BODY ;] IR-CTX:WITH-CONTEXT
   BND [: CROSS-BODY ;] IR-CTX:WITH-CONTEXT
   CDIGEST-DIGEST:EQ TTRUE ;

\ ---- the bound policy is part of the identity ---------------------------------
: POLICY-CASE ( -- )
   s" the same module under two numeric policies has two digests" T-LABEL
   BND [: CROSS-BODY ;] IR-CTX:WITH-CONTEXT
   BND-WRAP [: CROSS-BODY ;] IR-CTX:WITH-CONTEXT
   CDIGEST-DIGEST:EQ TFALSE ;

\ ---- program order stays observable ------------------------------------------
: SWAP-BODY ( IR-CTX:ctx -- n bool bool )
   {: c:IR-CTX:ctx :}
   c 0 0 ENC-OF {: pa pau:n :} \ typed-local-lint: allow-bare-local - pa keeps the ptr u8 byte-span role
   c 0 1 ENC-OF {: pb pbu:n :} \ typed-local-lint: allow-bare-local - pb keeps the ptr u8 byte-span role
   pau pbu -
   pa pau pb pbu SAME-BYTES?
   pa pau IR-ENCODE:DIGEST  pb pbu IR-ENCODE:DIGEST  CDIGEST-DIGEST:EQ ;

: SWAP-CASE ( -- )
   s" swapping two operations changes the bytes and the digest" T-LABEL
   BND [: SWAP-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TFALSE 0 T= ;

\ ---- encoding the same module twice gives the same bytes ---------------------
: REPEAT-BODY ( IR-CTX:ctx -- bool bool )
   {: c:IR-CTX:ctx :}
   c 0 0 TABLE-OF {: m:IR-BUILD:module t:IR-CANON:table :}
   c m t FRAME-OF {: pa pau:n :} \ typed-local-lint: allow-bare-local - pa keeps the ptr u8 byte-span role
   c m t FRAME-OF {: pb pbu:n :} \ typed-local-lint: allow-bare-local - pb keeps the ptr u8 byte-span role
   pa pau pb pbu SAME-BYTES?
   pa pau IR-ENCODE:DIGEST  pb pbu IR-ENCODE:DIGEST  CDIGEST-DIGEST:EQ ;

: REPEAT-CASE ( -- )
   s" re-encoding one module into a second span is byte identical" T-LABEL
   BND [: REPEAT-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE ;

\ ---- the frame's own layout, restated here on purpose ------------------------
\ These mirror src/compiler/ir/encode.f's private header layout. Restating them
\ is the point: a test that asked the encoder where its magic slot is would agree
\ with any layout the encoder happened to have, so the golden positions and the
\ golden header width live here, and moving a field in the encoder without
\ meaning to turns these fixtures red.
0 constant S-MAGIC
1 constant S-MAJOR
2 constant S-MINOR
14 constant S-SYMS
18 constant S-CELLS
152 constant HDR-BYTES               \ 19 slots of 8 bytes
32769 constant OVER-CEIL             \ one slot past the committed frame ceiling

\ ---- the frame is a header plus eight bytes per canonical slot ---------------
: WIDTH-BODY ( IR-CTX:ctx -- n n n )
   {: c:IR-CTX:ctx :}
   c 0 0 TABLE-OF {: m:IR-BUILD:module t:IR-CANON:table :}
   c m t FRAME-OF {: p len:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   t IR-CANON:CELLS {: k:n :}
   k
   t IR-ENCODE:SIZE len -
   len k 8 * - ;

: WIDTH-CASE ( -- )
   s" a frame is its header plus eight bytes per canonical slot" T-LABEL
   BND [: WIDTH-BODY ;] IR-CTX:WITH-CONTEXT
   HDR-BYTES T= 0 T= 0 T<> ;

\ ---- the header states what the module was built under ----------------------
\ Every header field is compared against the authority it came from, so a field
\ written to the wrong slot, or not written at all, is caught by name.
: HEAD-BODY ( IR-CTX:ctx -- n n n n n n n n bool bool )
   {: c:IR-CTX:ctx :}
   c 0 0 TABLE-OF {: m:IR-BUILD:module t:IR-CANON:table :}
   c m t FRAME-OF {: p len:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   m IR-BUILD:FSCHEMA-ROWS {: rows:IR-ARENA:view :}
   p len IR-ENCODE:FORMAT-MAJOR
   p len IR-ENCODE:FORMAT-MINOR
   p len IR-ENCODE:SCHEMA-MAJOR  rows IR-SCHEMA:FMAJOR@ -
   p len IR-ENCODE:SCHEMA-MINOR  rows IR-SCHEMA:FMINOR@ -
   p len IR-ENCODE:SYMBOLS  t IR-CANON:SYMBOLS -
   p len IR-ENCODE:TYPES  t IR-CANON:TYPES -
   p len IR-ENCODE:ATTRS  t IR-CANON:ATTRS -
   p len IR-ENCODE:SOURCES  t IR-CANON:SOURCES -
   p len IR-ENCODE:TARGET-DIGEST
   c IR-CTX:BINDING@ CBIND:TARGET@ CTARGET:DIGEST CDIGEST-DIGEST:EQ
   p len IR-ENCODE:POLICY-DIGEST
   c IR-CTX:BINDING@ CBIND:POLICY@ CNUM:DIGEST CDIGEST-DIGEST:EQ ;

: HEAD-CASE ( -- )
   s" the header states the bound target, policy, dialect and counts" T-LABEL
   BND [: HEAD-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE 0 T= 0 T= 0 T= 0 T= 0 T= 0 T= 0 T= 1 T= ;

\ ---- the dialect ordinal names the dialect's own symbol row ------------------
: DIALECT-BODY ( IR-CTX:ctx -- n )
   {: c:IR-CTX:ctx :}
   c 0 0 TABLE-OF {: m:IR-BUILD:module t:IR-CANON:table :}
   c m t FRAME-OF {: p len:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   m IR-BUILD:FSCHEMA-ROWS m IR-BUILD:FKEY IR-SCHEMA:FDIALECT@ {: d:IR-ID:ir-symbol-id :}
   p len IR-ENCODE:DIALECT  t d IR-CANON:SYMBOL-ORD - ;

: DIALECT-CASE ( -- )
   s" the header names the dialect's canonical symbol ordinal" T-LABEL
   BND [: DIALECT-BODY ;] IR-CTX:WITH-CONTEXT
   0 T= ;

\ ---- the payload is the canonical stream ------------------------------------
\ Slot for slot, in canon's own numbering. This is what makes the frame carry the
\ renumbered references rather than some other reading of the module.
: PAYLOAD-SAME? ( ptr u8 n IR-CANON:table -- bool )
   {: p len:n t:IR-CANON:table :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p len IR-ENCODE:CELLS {: k:n :}
   k t IR-CANON:CELLS <> if false exit then
   k 0 ?do
      p len i IR-ENCODE:CELL@  t i IR-CANON:CELL@  <> if false unloop exit then
   loop
   true ;

: PAYLOAD-BODY ( IR-CTX:ctx -- n bool )
   {: c:IR-CTX:ctx :}
   c 0 0 TABLE-OF {: m:IR-BUILD:module t:IR-CANON:table :}
   c m t FRAME-OF {: p len:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p len IR-ENCODE:CELLS  t IR-CANON:CELLS -
   p len t PAYLOAD-SAME? ;

: PAYLOAD-CASE ( -- )
   s" every payload slot is the canonical stream cell" T-LABEL
   BND [: PAYLOAD-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE 0 T= ;

\ ---- refusals ----------------------------------------------------------------
\ A destination one byte short of the frame.
: ROOM-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 0 TABLE-OF {: m:IR-BUILD:module t:IR-CANON:table :}
   t IR-ENCODE:SIZE {: len:n :}
   c len IR-CTX:SCRATCH-TAKE drop {: p :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   c m t p len 1- IR-ENCODE:ENCODE drop ;

: ROOM-RUN ( -- )
   BND [: ROOM-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A canonical table that numbers another module's rows.
: OWNER-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 0 TABLE-OF {: ma:IR-BUILD:module ta:IR-CANON:table :}
   c 0 0 MODULE-OF {: mb:IR-BUILD:module :}
   c mb ta FRAME-OF drop drop ;

: OWNER-RUN ( -- )
   BND [: OWNER-BODY ;] IR-CTX:WITH-CONTEXT ;

\ Bytes too short to hold a header, and bytes whose leading slot is not the magic.
: SHORT-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 0 ENC-OF drop {: p :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p HDR-BYTES 8 - IR-ENCODE:FRAME-CK ;

: SHORT-RUN ( -- )
   BND [: SHORT-BODY ;] IR-CTX:WITH-CONTEXT ;

: MAGIC-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 0 ENC-OF {: p len:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   0 p S-MAGIC CDIGEST:SLOT!
   p len IR-ENCODE:FRAME-CK ;

: MAGIC-RUN ( -- )
   BND [: MAGIC-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A frame of another format generation, in each direction that can be written.
: MAJOR-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 0 ENC-OF {: p len:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   99 p S-MAJOR CDIGEST:SLOT!
   p len IR-ENCODE:FRAME-CK ;

: MAJOR-RUN ( -- )
   BND [: MAJOR-BODY ;] IR-CTX:WITH-CONTEXT ;

: MINOR-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 0 ENC-OF {: p len:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   1 p S-MINOR CDIGEST:SLOT!
   p len IR-ENCODE:FRAME-CK ;

: MINOR-RUN ( -- )
   BND [: MINOR-BODY ;] IR-CTX:WITH-CONTEXT ;

\ Trailing bytes after the payload, and a payload one slot short of what the
\ header states: the same refusal, because neither span is one frame.
: TRAIL-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 0 ENC-OF {: p len:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p len 8 + IR-ENCODE:FRAME-CK ;

: TRAIL-RUN ( -- )
   BND [: TRAIL-BODY ;] IR-CTX:WITH-CONTEXT ;

: CUT-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 0 ENC-OF {: p len:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p len 8 - IR-ENCODE:FRAME-CK ;

: CUT-RUN ( -- )
   BND [: CUT-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A header claiming a payload past the committed frame ceiling: the decoder limit
\ a caller would otherwise size a buffer from.
: CEIL-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 0 ENC-OF {: p len:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   OVER-CEIL p S-CELLS CDIGEST:SLOT!
   p len IR-ENCODE:FRAME-CK ;

: CEIL-RUN ( -- )
   BND [: CEIL-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A header claiming more canonical symbol rows than its payload holds slots.
: ROWS-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 0 ENC-OF {: p len:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p len IR-ENCODE:CELLS 1+ p S-SYMS CDIGEST:SLOT!
   p len IR-ENCODE:FRAME-CK ;

: ROWS-RUN ( -- )
   BND [: ROWS-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A payload slot index at the slot count the frame states.
: BOUND-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 0 ENC-OF {: p len:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   p len  p len IR-ENCODE:CELLS  IR-ENCODE:CELL@ drop ;

: BOUND-RUN ( -- )
   BND [: BOUND-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A digest is only defined for a frame this encoder accepts.
: DIG-BAD-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 0 ENC-OF {: p len:n :} \ typed-local-lint: allow-bare-local - p keeps the ptr u8 byte-span role
   0 p S-MAGIC CDIGEST:SLOT!
   p len IR-ENCODE:DIGEST CDIGEST-DIGEST:UNMAKE drop drop drop drop ;

: DIG-BAD-RUN ( -- )
   BND [: DIG-BAD-BODY ;] IR-CTX:WITH-CONTEXT ;

: ROOM-CASE ( -- )
   s" a destination shorter than the frame rejects" T-LABEL
   [: ROOM-RUN ;] E-IR-ENCODE-ROOM TTHROWSQ ;

: OWNER-CASE ( -- )
   s" a canonical table of another module rejects" T-LABEL
   [: OWNER-RUN ;] E-IR-CANON-OWNER TTHROWSQ ;

: SHORT-CASE ( -- )
   s" bytes too short to hold a header reject" T-LABEL
   [: SHORT-RUN ;] E-IR-ENCODE-STATE TTHROWSQ ;

: MAGIC-CASE ( -- )
   s" bytes whose leading slot is not the magic reject" T-LABEL
   [: MAGIC-RUN ;] E-IR-ENCODE-STATE TTHROWSQ ;

: MAJOR-CASE ( -- )
   s" a frame of another format major version rejects" T-LABEL
   [: MAJOR-RUN ;] E-IR-ENCODE-VERSION TTHROWSQ ;

: MINOR-CASE ( -- )
   s" a frame of a later format minor version rejects" T-LABEL
   [: MINOR-RUN ;] E-IR-ENCODE-VERSION TTHROWSQ ;

: TRAIL-CASE ( -- )
   s" bytes trailing the payload reject" T-LABEL
   [: TRAIL-RUN ;] E-IR-ENCODE-FRAME TTHROWSQ ;

: CUT-CASE ( -- )
   s" a payload shorter than the header states rejects" T-LABEL
   [: CUT-RUN ;] E-IR-ENCODE-FRAME TTHROWSQ ;

: CEIL-CASE ( -- )
   s" a payload past the committed frame ceiling rejects" T-LABEL
   [: CEIL-RUN ;] E-IR-ENCODE-CAP TTHROWSQ ;

: ROWS-CASE ( -- )
   s" more canonical rows than the payload could hold rejects" T-LABEL
   [: ROWS-RUN ;] E-IR-ENCODE-CAP TTHROWSQ ;

: BOUND-CASE ( -- )
   s" a payload index at the stated slot count rejects" T-LABEL
   [: BOUND-RUN ;] E-IR-ENCODE-BOUND TTHROWSQ ;

: DIG-BAD-CASE ( -- )
   s" a digest of bytes that are not a frame rejects" T-LABEL
   [: DIG-BAD-RUN ;] E-IR-ENCODE-STATE TTHROWSQ ;

public

\ Each case owns the contexts it needs; the outer context is the one every other
\ IR test opens around a case, so a registry sweep always has a live context to
\ observe.
: ORDER-CASES ( -- )
   BND [: drop SCHEMA-DIG-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop EQUIV-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop CROSS-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop POLICY-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop SWAP-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop REPEAT-CASE ;] IR-CTX:WITH-CONTEXT ;

: HEADER-CASES ( -- )
   BND [: drop WIDTH-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop HEAD-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop DIALECT-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop PAYLOAD-CASE ;] IR-CTX:WITH-CONTEXT ;

: REFUSAL-CASES ( -- )
   BND [: drop ROOM-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop OWNER-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop SHORT-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop MAGIC-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop MAJOR-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop MINOR-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop TRAIL-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop CUT-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop CEIL-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop ROWS-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop BOUND-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop DIG-BAD-CASE ;] IR-CTX:WITH-CONTEXT ;

: RUN ( -- )
   T-RESET
   ORDER-CASES
   HEADER-CASES
   REFUSAL-CASES
   T-REPORT ;

;package

IR-ENCODE-TEST:RUN

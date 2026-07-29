\ ir-canon.f - checked canonical table order tests.
\
\ Proves the sections 5.7 and 6.6 contract of src/compiler/ir/canon.f: a frozen
\ module's canonical stream depends on what the module means and not on the
\ order its tables were interned in, while everything the module orders on
\ purpose stays observable.
\
\ HOW THE MAIN FIXTURE WORKS. BUILD builds one module through the real IR-BUILD
\ API and takes a flag that reverses the insertion order of every interned
\ table: the two integer types, the two tag symbols, and the attribute values.
\ The reversal stays inside the admissible orders - the pointer is still interned
\ after its pointee and the record after its values, because IR-TYPE and IR-ATTR
\ refuse anything else - so the two modules are the same module built along two
\ topological orders of the same reference graph. The type group is the exact
\ counterexample formal/Common/Interning.v carries: i8 and i16 in both orders
\ with a pointer to i8 either way, whose stored rows are provably not a
\ permutation of each other while their denotations agree. The fixture first
\ measures that the reversal really did move the insertion ordinals, because a
\ fixture that reversed nothing would pass for the wrong reason, and then
\ requires the two canonical streams to be equal cell for cell.
\
\ WHAT THE OTHER FIXTURES ADD. One pins the order itself, so the test says what
\ the canonical order IS rather than only that two modules agree on it: symbols
\ sorted by their bytes, types by kind and then by field, attributes by kind and
\ then by value. One swaps two operations and requires the streams to differ,
\ which is the other half of the contract - canonicalization renumbers tables
\ and must not reorder a program. One registers the same source bytes twice and
\ requires the two rows to share one canonical ordinal, which is what makes the
\ canonical source table content-addressed. The rest are the refusals, one
\ fixture per named error that a checked caller can reach.

require lib/test.f
require src/compiler/ir/build.f
require src/compiler/ir/canon.f

package IR-CANON-TEST
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

\ A pointer to i8: its pointee's ordinal is what the type table stores, so this
\ is the row the two insertion orders disagree about.
: PTR8 ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  IR--TYPE-SPACE:GENERIC  c b I8  IR-BUILD:INTERN-POINTER ;

\ A code reference (i64 -- i64): its row names a window of type ordinals.
: SIGT ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-type-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b I64 {: ty:IR-ID:ir-type-id :}
   IR-TYPE:FN-BEGIN
   ty IR-TYPE:FN-PARAM
   ty IR-TYPE:FN-RESULT
   c b IR-BUILD:INTERN-CODE-REF ;

\ ---- the two tag symbols -----------------------------------------------------
\ Lexicographically first and last, so their canonical order is the reverse of
\ one insertion order and the same as the other.
: A-TAG ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" a-tag" IR-BUILD:INTERN-SYMBOL ;

: Z-TAG ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" z-tag" IR-BUILD:INTERN-SYMBOL ;

\ ---- which order this module is being built along ----------------------------
\ The order groups below and the spans both need it, and threading it through
\ every operation helper would say nothing extra, so one fixture-owned cell
\ holds it for the length of one build.
variable REV-CELL

\ ---- sources -----------------------------------------------------------------
\ The source registry does not deduplicate, so a fixture that registered a
\ source per span would fill the table with copies and every span would name a
\ row of its own - and the two builds would then agree about span ordinals for
\ the wrong reason. These two are registered once by the order group, and a span
\ names the row that group gave them, which is a different row in the two
\ builds.
: SRC-ONE+ ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-source-id )
   s" source-one" IR-BUILD:ADD-SOURCE ;

: SRC-TWO+ ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-source-id )
   s" source-two-longer" IR-BUILD:ADD-SOURCE ;

: SRC-ONE ( IR-BUILD:builder -- IR-ID:ir-source-id )
   {: b:IR-BUILD:builder :}
   b IR-BUILD:MODULE-KEY  REV-CELL @ 0= if 0 else 1 then  IR-ID:PACK-SOURCE ;

: SRC-TWO ( IR-BUILD:builder -- IR-ID:ir-source-id )
   {: b:IR-BUILD:builder :}
   b IR-BUILD:MODULE-KEY  REV-CELL @ 0= if 1 else 0 then  IR-ID:PACK-SOURCE ;

\ ---- the dialect -------------------------------------------------------------
\ Five opcodes: a constant, a tagged constant carrying both attribute keys, a
\ branch that hands its destination one argument, a use, and a return.
0 constant K-CONST
1 constant K-TAGGED
2 constant K-BR1
3 constant K-USE
4 constant K-RET

: OPC-NAME ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:n :}
   k K-CONST = if c b s" hir.const" IR-BUILD:INTERN-SYMBOL exit then
   k K-TAGGED = if c b s" hir.tagged" IR-BUILD:INTERN-SYMBOL exit then
   k K-BR1 = if c b s" hir.br1" IR-BUILD:INTERN-SYMBOL exit then
   k K-USE = if c b s" hir.use" IR-BUILD:INTERN-SYMBOL exit then
   c b s" hir.ret" IR-BUILD:INTERN-SYMBOL ;

: SCH-VALUE ( IR-CTX:ctx IR-BUILD:builder n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:n :}
   k K-CONST = if c b I64 IR-SCHEMA:ADD-RESULT then
   k K-USE = if c b I64 IR-SCHEMA:ADD-OPERAND then
   k K-BR1 = if c b I64 IR-SCHEMA:ADD-OPERAND then
   k K-TAGGED = if
      c b I64 IR-SCHEMA:ADD-RESULT
      c b Z-TAG IR-SCHEMA:ADD-ATTR
      c b A-TAG IR-SCHEMA:ADD-ATTR
   then ;

: SCH-CTRL ( n -- )
   {: k:n :}
   k K-RET = if true 0 0 IR-SCHEMA:SET-CONTROL exit then
   k K-BR1 = if true 1 0 IR-SCHEMA:SET-CONTROL exit then
   false 0 0 IR-SCHEMA:SET-CONTROL ;

: SCH-DEF ( IR-CTX:ctx IR-BUILD:builder n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder k:n :}
   c b k OPC-NAME IR-SCHEMA:BEGIN-OP
   c b k SCH-VALUE
   k SCH-CTRL
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
   c b K-BR1 SCH-DEF
   c b K-USE SCH-DEF
   c b K-RET SCH-DEF ;

\ ---- the interned tables, in either admissible order -------------------------
\ Each group is one list walked forwards or backwards. Walking backwards is
\ still an admissible order, because a member that references another interns
\ that other one on the way in: interning the code reference first is what
\ interns i64 first, and interning the record first is what interns its integer
\ values first. Every group is chosen so that reversing it really moves the
\ ordinals the stream has to renumber - a group with a fixed point in the middle
\ would let a missing renumbering pass unnoticed - and EQUIV-BODY measures four
\ of those moves rather than trusting them.
: MAIN-SYM ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" main" IR-BUILD:INTERN-SYMBOL ;

: RULE-SYM ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" rule.hir" IR-BUILD:INTERN-SYMBOL ;

: RENDER-SYM ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" render.hir" IR-BUILD:INTERN-SYMBOL ;

10 constant SYM#

: NTH-SYM ( IR-CTX:ctx IR-BUILD:builder n -- IR-ID:ir-symbol-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder n:n :}
   n 0 = if c b MAIN-SYM exit then
   n 1 = if c b A-TAG exit then
   n 2 = if c b Z-TAG exit then
   n 8 = if c b RULE-SYM exit then
   n 9 = if c b RENDER-SYM exit then
   c b n 3 - OPC-NAME ;

\ Ten symbols in one order or its reverse: with an even count no symbol keeps its
\ ordinal, so the function name, every opcode name and both tags move.
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

: SYM-A ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b Z-TAG  IR-BUILD:INTERN-SYMBOL-ATTR ;

\ A type reference to i8, whose type ordinal is one the two builds disagree
\ about, so the attribute table's type renumbering has something to get wrong.
: TYPE-A ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b  c b I8  IR-BUILD:INTERN-TYPE-ATTR ;

\ A record whose two keys are the two tag symbols: its stored pairs are sorted by
\ the key's insertion ordinal, so the two modules store them in opposite orders
\ and the canonical pairs still have to agree.
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
      c b SYM-A drop
      c b TYPE-A drop
      c b REC-A drop
      exit
   then
   c b REC-A drop
   c b TYPE-A drop
   c b SYM-A drop
   c b TXT-A drop
   c b INT-3 drop
   c b INT7 drop ;

: SRCS-IN ( IR-CTX:ctx IR-BUILD:builder n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder rev:n :}
   rev 0= if
      c b SRC-ONE+ drop
      c b SRC-TWO+ drop
      exit
   then
   c b SRC-TWO+ drop
   c b SRC-ONE+ drop ;

\ ---- spans -------------------------------------------------------------------
\ Both spans name a source the order group already registered, so the source
\ table stays the two rows that group put in it and a span's stored source
\ ordinal is one the two builds disagree about.
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
\ are keyed, so design line 479 makes the order they were added in no part of the
\ operation: the two builds add them oppositely and the canonical stream still
\ has to state them once, sorted by canonical key.
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

: BR1+ ( IR-CTX:ctx IR-BUILD:builder IR-ID:ir-value-id IR-ID:ir-block-id -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder v:IR-ID:ir-value-id t:IR-ID:ir-block-id :}
   c b K-BR1 OP-OPEN
   c b v IR-BUILD:ADD-OPERAND
   c b t IR-BUILD:ADD-SUCCESSOR
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

: BLK-ID ( IR-BUILD:builder n -- IR-ID:ir-block-id )
   {: b:IR-BUILD:builder ord:n :}
   b IR-BUILD:MODULE-KEY ord IR-ID:PACK-BLOCK ;

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

: BLK-OPEN ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b IR-BUILD:BEGIN-BLOCK
   c b  b SPAN-ONE  IR-BUILD:SET-BLOCK-SPAN ;

\ The entry block defines a constant, a tagged constant, and branches to the
\ second block with the constant as its argument; the second block uses its
\ argument and returns.
: ENTRY-OPS ( IR-CTX:ctx IR-BUILD:builder n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder swapped:n :}
   swapped 0= if
      c b CONST+ {: v:IR-ID:ir-value-id :}
      c b TAGGED+
      c b v  b 1 BLK-ID  BR1+
      exit
   then
   c b TAGGED+
   c b CONST+ {: v:IR-ID:ir-value-id :}
   c b v  b 1 BLK-ID  BR1+ ;

: TAIL-BLOCK ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   c b BLK-OPEN
   c b  c b I64  IR-BUILD:ADD-BLOCK-ARG {: a:IR-ID:ir-value-id :}
   c b a USE+
   c b RET+
   c b IR-BUILD:END-BLOCK drop ;

\ One whole module: rev picks the insertion order of every interned table, and
\ swapped picks the operation order inside the entry block.
: BUILD ( IR-CTX:ctx IR-BUILD:builder n n -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder rev:n swapped:n :}
   rev REV-CELL !
   c b rev SYMS-IN
   c b rev TYPES-IN
   c b rev ATTRS-IN
   c b rev SRCS-IN
   c b SCH-ALL
   c b FN-OPEN
   c b BLK-OPEN
   c b swapped ENTRY-OPS
   c b IR-BUILD:END-BLOCK drop
   c b TAIL-BLOCK
   c b IR-BUILD:END-FUN drop ;

: MODULE-OF ( IR-CTX:ctx n n -- IR-BUILD:module )
   {: c:IR-CTX:ctx rev:n swapped:n :}
   c MK {: b:IR-BUILD:builder :}
   c b rev swapped BUILD
   c b IR-BUILD:FREEZE ;

\ ---- comparing two canonical streams -----------------------------------------
: SAME-STREAM? ( IR-CANON:table IR-CANON:table -- bool )
   {: x:IR-CANON:table y:IR-CANON:table :}
   x IR-CANON:CELLS y IR-CANON:CELLS <> if false exit then
   x IR-CANON:CELLS 0 ?do
      x i IR-CANON:CELL@  y i IR-CANON:CELL@  <> if false unloop exit then
   loop
   true ;

\ ---- the equivalence fixture -------------------------------------------------
\ Two modules along two topological orders. The first three answers measure that
\ the reversal really moved the insertion ordinals, and the last two are the
\ contract: the streams are the same length and the same cells.
: EQUIV-BODY ( IR-CTX:ctx -- n n n bool bool )
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
   c ma IR-CANON:CANON {: ta:IR-CANON:table :}
   c mb IR-CANON:CANON {: tb:IR-CANON:table :}
   i8a i8b -
   taga tagb -
   ta IR-CANON:CELLS tb IR-CANON:CELLS -
   ta IR-CANON:CELLS 0 >
   ta tb SAME-STREAM? ;

\ The two insertion orders really differ, the streams are the same length and
\ not empty, and every cell agrees.
: EQUIV-CASE ( -- )
   s" two topological build orders canonicalize to the same stream" T-LABEL
   BND [: EQUIV-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE 0 T= 0 T<> 0 T<> ;

\ ---- the canonical order itself ----------------------------------------------
\ Symbols are ordered by their bytes, so "a-tag" is first and "z-tag" is last of
\ the eleven this module interns. Types are ordered by kind and then by field:
\ the three integers by width, then the pointer, then the code reference.
: ORDER-BODY ( IR-CTX:ctx -- n n n n n n n n n )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b 1 0 BUILD
   c b I8 {: t8:IR-ID:ir-type-id :}
   c b I16 {: t16:IR-ID:ir-type-id :}
   c b I64 {: t64:IR-ID:ir-type-id :}
   c b PTR8 {: tp:IR-ID:ir-type-id :}
   c b SIGT {: tc:IR-ID:ir-type-id :}
   c b A-TAG {: sa:IR-ID:ir-symbol-id :}
   c b Z-TAG {: sz:IR-ID:ir-symbol-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   c m IR-CANON:CANON {: t:IR-CANON:table :}
   t IR-CANON:SYMBOLS
   t sa IR-CANON:SYMBOL-ORD
   t sz IR-CANON:SYMBOL-ORD
   t IR-CANON:TYPES
   t t8 IR-CANON:TYPE-ORD
   t t16 IR-CANON:TYPE-ORD
   t t64 IR-CANON:TYPE-ORD
   t tp IR-CANON:TYPE-ORD
   t tc IR-CANON:TYPE-ORD ;

: ORDER-CASE ( -- )
   s" symbols order by bytes and types by kind then field" T-LABEL
   BND [: ORDER-BODY ;] IR-CTX:WITH-CONTEXT
   4 T= 3 T= 2 T= 1 T= 0 T= 5 T= 10 T= 0 T= 11 T= ;

\ ---- attributes order by kind and then by value ------------------------------
\ The two integers by value, then the string, the symbol reference, the type
\ reference, and the record, which is IR-ATTR's kind order.
: ATTR-ORDER-BODY ( IR-CTX:ctx -- n n n n n n n )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b 1 0 BUILD
   c b INT7 {: a7:IR-ID:ir-attr-id :}
   c b INT-3 {: a3:IR-ID:ir-attr-id :}
   c b TXT-A {: at:IR-ID:ir-attr-id :}
   c b SYM-A {: as:IR-ID:ir-attr-id :}
   c b TYPE-A {: ay:IR-ID:ir-attr-id :}
   c b REC-A {: ar:IR-ID:ir-attr-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   c m IR-CANON:CANON {: t:IR-CANON:table :}
   t IR-CANON:ATTRS
   t a3 IR-CANON:ATTR-ORD
   t a7 IR-CANON:ATTR-ORD
   t at IR-CANON:ATTR-ORD
   t as IR-CANON:ATTR-ORD
   t ay IR-CANON:ATTR-ORD
   t ar IR-CANON:ATTR-ORD ;

: ATTR-ORDER-CASE ( -- )
   s" attributes order by kind and then by value" T-LABEL
   BND [: ATTR-ORDER-BODY ;] IR-CTX:WITH-CONTEXT
   5 T= 4 T= 3 T= 2 T= 1 T= 0 T= 6 T= ;

\ ---- a swapped pair of operations stays observable ---------------------------
\ The two modules intern everything in the same order and differ only in the
\ order of the first two operations of the entry block, which is program order
\ and not numbering. The streams are the same length and must not be equal.
: SWAP-BODY ( IR-CTX:ctx -- n bool )
   {: c:IR-CTX:ctx :}
   c 0 0 MODULE-OF {: ma:IR-BUILD:module :}
   c 0 1 MODULE-OF {: mb:IR-BUILD:module :}
   c ma IR-CANON:CANON {: ta:IR-CANON:table :}
   c mb IR-CANON:CANON {: tb:IR-CANON:table :}
   ta IR-CANON:CELLS tb IR-CANON:CELLS -
   ta tb SAME-STREAM? ;

: SWAP-CASE ( -- )
   s" swapping two operations changes the canonical stream" T-LABEL
   BND [: SWAP-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE 0 T= ;

\ ---- two registrations of one source share one canonical ordinal -------------
\ The source registry deliberately does not deduplicate, so the same bytes
\ registered twice are two rows with one content. The canonical table is content
\ addressed, so both rows answer the same ordinal and the canonical count is one
\ short of the registry's.
: MERGE-BODY ( IR-CTX:ctx -- n n n n )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b 0 0 BUILD
   c b SRC-ONE+ {: dup1:IR-ID:ir-source-id :}
   b SRC-ONE {: one:IR-ID:ir-source-id :}
   b SRC-TWO {: two:IR-ID:ir-source-id :}
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   c m IR-CANON:CANON {: t:IR-CANON:table :}
   dup1 IR-ID:SOURCE-LOCAL one IR-ID:SOURCE-LOCAL -
   t IR-CANON:SOURCES
   t one IR-CANON:SOURCE-ORD  t dup1 IR-CANON:SOURCE-ORD -
   t one IR-CANON:SOURCE-ORD  t two IR-CANON:SOURCE-ORD - ;

\ The two rows are distinct identities, the canonical table holds two sources
\ rather than three, the duplicate pair share an ordinal, and the source with
\ other content does not.
: MERGE-CASE ( -- )
   s" the same source bytes registered twice share one canonical ordinal" T-LABEL
   BND [: MERGE-BODY ;] IR-CTX:WITH-CONTEXT
   0 T<> 0 T= 2 T= 0 T<> ;

\ ---- a canonical table is live until it is released --------------------------
: LIFE-BODY ( IR-CTX:ctx -- bool bool )
   {: c:IR-CTX:ctx :}
   c 0 0 MODULE-OF {: m:IR-BUILD:module :}
   c m IR-CANON:CANON {: t:IR-CANON:table :}
   t IR-CANON:LIVE?
   t IR-CANON:RELEASE
   t IR-CANON:LIVE? ;

: LIFE-CASE ( -- )
   s" a canonical table is live until it is released" T-LABEL
   BND [: LIFE-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TTRUE ;

\ ---- refusals ----------------------------------------------------------------
\ A released table: the store is retired, so every reader is named rather than
\ reading a retired arena.
: RELEASED-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 0 MODULE-OF {: m:IR-BUILD:module :}
   c m IR-CANON:CANON {: t:IR-CANON:table :}
   t IR-CANON:RELEASE
   t IR-CANON:CELLS drop ;

: RELEASED-RUN ( -- )
   BND [: RELEASED-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A stream index at the length itself.
: BOUND-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 0 MODULE-OF {: m:IR-BUILD:module :}
   c m IR-CANON:CANON {: t:IR-CANON:table :}
   t  t IR-CANON:CELLS  IR-CANON:CELL@ drop ;

: BOUND-RUN ( -- )
   BND [: BOUND-BODY ;] IR-CTX:WITH-CONTEXT ;

\ An identity another module minted. Two modules of one context have different
\ module keys, so the second module's symbol zero is not a row of the first
\ module's canonical table.
: OWNER-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 0 MODULE-OF {: ma:IR-BUILD:module :}
   c 0 0 MODULE-OF {: mb:IR-BUILD:module :}
   c ma IR-CANON:CANON {: t:IR-CANON:table :}
   t  mb IR-BUILD:FKEY 0 IR-ID:PACK-SYMBOL  IR-CANON:SYMBOL-ORD drop ;

: OWNER-RUN ( -- )
   BND [: OWNER-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A name longer than the committed working set. The refusal runs before the
\ canonical store is created, so it costs the context nothing.
create BIG-NAME 300 allot

: BIG-NAME-FILL ( -- )
   300 0 ?do
      $61 BIG-NAME i + c!
   loop ;

: CAP-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c MK {: b:IR-BUILD:builder :}
   c b 0 0 BUILD
   BIG-NAME-FILL
   c b BIG-NAME 300 IR-BUILD:INTERN-SYMBOL drop
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   c m IR-CANON:CANON drop ;

: CAP-RUN ( -- )
   BND [: CAP-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A module whose own context has torn down. Its tables are unmapped, so it is no
\ longer a frozen module anything may read.
: INNER-MODULE ( IR-CTX:ctx -- IR-BUILD:module )
   0 0 MODULE-OF ;

: STALE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   BND [: INNER-MODULE ;] IR-CTX:WITH-CONTEXT {: m:IR-BUILD:module :}
   c m IR-CANON:CANON drop ;

: STALE-RUN ( -- )
   BND [: STALE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ One canonical table more than the registry holds.
: CANON-DROP ( IR-CTX:ctx IR-BUILD:module -- )
   IR-CANON:CANON drop ;

: SLOTS-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 0 MODULE-OF {: m:IR-BUILD:module :}
   9 0 ?do
      c m CANON-DROP
   loop ;

: SLOTS-RUN ( -- )
   BND [: SLOTS-BODY ;] IR-CTX:WITH-CONTEXT ;

: RELEASED-CASE ( -- )
   s" a reader of a released canonical table rejects" T-LABEL
   [: RELEASED-RUN ;] E-IR-CANON-RELEASED TTHROWSQ ;

: BOUND-CASE ( -- )
   s" a stream index at the canonical length rejects" T-LABEL
   [: BOUND-RUN ;] E-IR-CANON-BOUND TTHROWSQ ;

: OWNER-CASE ( -- )
   s" an identity another module minted rejects" T-LABEL
   [: OWNER-RUN ;] E-IR-CANON-OWNER TTHROWSQ ;

: CAP-CASE ( -- )
   s" a name longer than the committed working set rejects" T-LABEL
   [: CAP-RUN ;] E-IR-CANON-CAP TTHROWSQ ;

: STALE-CASE ( -- )
   s" a module whose context has torn down rejects" T-LABEL
   [: STALE-RUN ;] E-IR-CANON-STALE TTHROWSQ ;

: SLOTS-CASE ( -- )
   s" one canonical table more than the registry holds rejects" T-LABEL
   [: SLOTS-RUN ;] E-IR-CANON-SLOTS TTHROWSQ ;

public

: RUN ( -- )
   T-RESET
   BND [: drop ORDER-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop ATTR-ORDER-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop EQUIV-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop SWAP-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop MERGE-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop LIFE-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop RELEASED-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop BOUND-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop OWNER-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop CAP-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop STALE-CASE ;] IR-CTX:WITH-CONTEXT
   BND [: drop SLOTS-CASE ;] IR-CTX:WITH-CONTEXT
   T-REPORT ;

;package

IR-CANON-TEST:RUN

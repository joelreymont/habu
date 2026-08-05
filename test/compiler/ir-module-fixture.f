\ ir-module-fixture.f - one frozen module, built two ways, for every test that
\ needs one.
\
\ WHY THIS IS ITS OWN FILE. test/compiler/ir-canon.f built this module to prove
\ that canonicalization does not depend on interning order: BUILD takes a flag
\ that reverses the insertion order of every interned table while staying inside
\ the admissible orders, and a second flag that swaps two operations of the entry
\ block so program order stays observable. The renderer and the structural diff
\ need exactly the same module for exactly the same reason - a golden that
\ depended on build order would be worthless - so the builder lives here and its
\ three callers use it rather than each keeping a copy of it.
\
\ WHAT THE MODULE CONTAINS. Ten interned symbols (a function name, five opcode
\ names, two tag symbols, a rule name and a renderer name), five types (three
\ integers, a pointer to the narrowest one, and a code reference whose row names a
\ window of type ordinals), six attributes (two integers, a string, a symbol
\ reference, a type reference, and a record keyed by both tags), two registered
\ sources, a five-opcode dialect, and one function of two blocks: the entry block
\ defines a constant and a tagged constant and branches to the second block with
\ the constant as its argument, and the second block uses its argument and
\ returns.
\
\ THE FOUR KNOBS, AND WHAT EACH ONE IS FOR.
\
\   the reverse flag of BUILD walks every interned group backwards, which is a
\   second topological order of the same reference graph: the pointer is still
\   interned after its pointee and the record after its values, because IR-TYPE
\   and IR-ATTR refuse anything else. Two modules built this way mean the same
\   thing and store different ordinals, which is what a canonical form and a
\   deterministic render have to agree about.
\
\   the swap flag of BUILD exchanges the first two operations of the entry block.
\   That is program order, not numbering, so it must stay visible.
\
\   EXTRA-SYMBOL! interns one more symbol whose bytes sort near the front, so
\   every later canonical ordinal shifts by one while the module otherwise means
\   the same thing. A structural diff must report one added symbol; a diff that
\   compared rendered text would report every row whose number moved.
\
\   CHANGED-ATTR! makes the tagged operation carry the other integer under its
\   z-tag key. Exactly one operation row differs and no table row does, which is
\   the smallest semantic change this module can carry.
\
\ Both knobs stay off until a caller sets them and RESET puts them back, so a
\ caller that never mentions them builds the module test/compiler/ir-canon.f has
\ always built.

require lib/errors.f
require src/compiler/ir/build.f
require src/compiler/ir/canon.f

package IR-FIXTURE
public

\ ---- the knobs ---------------------------------------------------------------
variable EXTRA-CELL
variable CHANGED-CELL

: RESET ( -- )
   0 EXTRA-CELL !
   0 CHANGED-CELL ! ;

: EXTRA-SYMBOL! ( -- )
   1 EXTRA-CELL ! ;

: CHANGED-ATTR! ( -- )
   1 CHANGED-CELL ! ;

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

\ The extra symbol EXTRA-SYMBOL! adds. Its bytes sort second of the eleven, so
\ interning it shifts every later canonical ordinal by one and nothing else about
\ the module changes.
: B-TAG ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-symbol-id )
   s" b-tag" IR-BUILD:INTERN-SYMBOL ;

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
   loop
   EXTRA-CELL @ 0= if exit then
   c b B-TAG drop ;

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
\ Which integer the z-tag key carries. CHANGED-ATTR! picks the other one, which
\ is the smallest semantic change this module can carry: one operation row
\ differs and no table row does, because both integers are interned either way.
: Z-VALUE ( IR-CTX:ctx IR-BUILD:builder -- IR-ID:ir-attr-id )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   CHANGED-CELL @ 0= if c b INT7 exit then
   c b INT-3 ;

: TAG-ATTRS ( IR-CTX:ctx IR-BUILD:builder -- )
   {: c:IR-CTX:ctx b:IR-BUILD:builder :}
   REV-CELL @ 0= if
      c b  c b Z-TAG  c b Z-VALUE  IR-BUILD:ADD-ATTR
      c b  c b A-TAG  c b INT-3  IR-BUILD:ADD-ATTR
      exit
   then
   c b  c b A-TAG  c b INT-3  IR-BUILD:ADD-ATTR
   c b  c b Z-TAG  c b Z-VALUE  IR-BUILD:ADD-ATTR ;

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

;package

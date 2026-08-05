\ ir-canon.f - checked canonical table order tests.
\
\ Proves the sections 5.7 and 6.6 contract of src/compiler/ir/canon.f: a frozen
\ module's canonical stream depends on what the module means and not on the
\ order its tables were interned in, while everything the module orders on
\ purpose stays observable.
\
\ HOW THE MAIN FIXTURE WORKS. The module itself is built by
\ test/compiler/ir-module-fixture.f, which the renderer and structural-diff tests
\ share with this one. IR-FIXTURE:BUILD builds one module through the real
\ IR-BUILD API and takes a flag that reverses the insertion order of every interned
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
require test/compiler/ir-module-fixture.f

package IR-CANON-TEST
private

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
   c IR-FIXTURE:MK {: ba:IR-BUILD:builder :}
   c ba 0 0 IR-FIXTURE:BUILD
   c ba IR-FIXTURE:I8 IR-ID:TYPE-LOCAL {: i8a:n :}
   c ba IR-FIXTURE:A-TAG IR-ID:SYMBOL-LOCAL {: taga:n :}
   c ba IR-BUILD:FREEZE {: ma:IR-BUILD:module :}
   c IR-FIXTURE:MK {: bb:IR-BUILD:builder :}
   c bb 1 0 IR-FIXTURE:BUILD
   c bb IR-FIXTURE:I8 IR-ID:TYPE-LOCAL {: i8b:n :}
   c bb IR-FIXTURE:A-TAG IR-ID:SYMBOL-LOCAL {: tagb:n :}
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
   IR-FIXTURE:BND [: EQUIV-BODY ;] IR-CTX:WITH-CONTEXT
   TTRUE TTRUE 0 T= 0 T<> 0 T<> ;

\ ---- the canonical order itself ----------------------------------------------
\ Symbols are ordered by their bytes, so "a-tag" is first and "z-tag" is last of
\ the eleven this module interns. Types are ordered by kind and then by field:
\ the three integers by width, then the pointer, then the code reference.
: ORDER-BODY ( IR-CTX:ctx -- n n n n n n n n n )
   {: c:IR-CTX:ctx :}
   c IR-FIXTURE:MK {: b:IR-BUILD:builder :}
   c b 1 0 IR-FIXTURE:BUILD
   c b IR-FIXTURE:I8 {: t8:IR-ID:ir-type-id :}
   c b IR-FIXTURE:I16 {: t16:IR-ID:ir-type-id :}
   c b IR-FIXTURE:I64 {: t64:IR-ID:ir-type-id :}
   c b IR-FIXTURE:PTR8 {: tp:IR-ID:ir-type-id :}
   c b IR-FIXTURE:SIGT {: tc:IR-ID:ir-type-id :}
   c b IR-FIXTURE:A-TAG {: sa:IR-ID:ir-symbol-id :}
   c b IR-FIXTURE:Z-TAG {: sz:IR-ID:ir-symbol-id :}
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
   IR-FIXTURE:BND [: ORDER-BODY ;] IR-CTX:WITH-CONTEXT
   4 T= 3 T= 2 T= 1 T= 0 T= 5 T= 10 T= 0 T= 11 T= ;

\ ---- attributes order by kind and then by value ------------------------------
\ The two integers by value, then the string, the symbol reference, the type
\ reference, and the record, which is IR-ATTR's kind order.
: ATTR-ORDER-BODY ( IR-CTX:ctx -- n n n n n n n )
   {: c:IR-CTX:ctx :}
   c IR-FIXTURE:MK {: b:IR-BUILD:builder :}
   c b 1 0 IR-FIXTURE:BUILD
   c b IR-FIXTURE:INT7 {: a7:IR-ID:ir-attr-id :}
   c b IR-FIXTURE:INT-3 {: a3:IR-ID:ir-attr-id :}
   c b IR-FIXTURE:TXT-A {: at:IR-ID:ir-attr-id :}
   c b IR-FIXTURE:SYM-A {: as:IR-ID:ir-attr-id :}
   c b IR-FIXTURE:TYPE-A {: ay:IR-ID:ir-attr-id :}
   c b IR-FIXTURE:REC-A {: ar:IR-ID:ir-attr-id :}
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
   IR-FIXTURE:BND [: ATTR-ORDER-BODY ;] IR-CTX:WITH-CONTEXT
   5 T= 4 T= 3 T= 2 T= 1 T= 0 T= 6 T= ;

\ ---- a swapped pair of operations stays observable ---------------------------
\ The two modules intern everything in the same order and differ only in the
\ order of the first two operations of the entry block, which is program order
\ and not numbering. The streams are the same length and must not be equal.
: SWAP-BODY ( IR-CTX:ctx -- n bool )
   {: c:IR-CTX:ctx :}
   c 0 0 IR-FIXTURE:MODULE-OF {: ma:IR-BUILD:module :}
   c 0 1 IR-FIXTURE:MODULE-OF {: mb:IR-BUILD:module :}
   c ma IR-CANON:CANON {: ta:IR-CANON:table :}
   c mb IR-CANON:CANON {: tb:IR-CANON:table :}
   ta IR-CANON:CELLS tb IR-CANON:CELLS -
   ta tb SAME-STREAM? ;

: SWAP-CASE ( -- )
   s" swapping two operations changes the canonical stream" T-LABEL
   IR-FIXTURE:BND [: SWAP-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE 0 T= ;

\ ---- two registrations of one source share one canonical ordinal -------------
\ The source registry deliberately does not deduplicate, so the same bytes
\ registered twice are two rows with one content. The canonical table is content
\ addressed, so both rows answer the same ordinal and the canonical count is one
\ short of the registry's.
: MERGE-BODY ( IR-CTX:ctx -- n n n n )
   {: c:IR-CTX:ctx :}
   c IR-FIXTURE:MK {: b:IR-BUILD:builder :}
   c b 0 0 IR-FIXTURE:BUILD
   c b IR-FIXTURE:SRC-ONE+ {: dup1:IR-ID:ir-source-id :}
   b IR-FIXTURE:SRC-ONE {: one:IR-ID:ir-source-id :}
   b IR-FIXTURE:SRC-TWO {: two:IR-ID:ir-source-id :}
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
   IR-FIXTURE:BND [: MERGE-BODY ;] IR-CTX:WITH-CONTEXT
   0 T<> 0 T= 2 T= 0 T<> ;

\ ---- a canonical table is live until it is released --------------------------
: LIFE-BODY ( IR-CTX:ctx -- bool bool )
   {: c:IR-CTX:ctx :}
   c 0 0 IR-FIXTURE:MODULE-OF {: m:IR-BUILD:module :}
   c m IR-CANON:CANON {: t:IR-CANON:table :}
   t IR-CANON:LIVE?
   t IR-CANON:RELEASE
   t IR-CANON:LIVE? ;

: LIFE-CASE ( -- )
   s" a canonical table is live until it is released" T-LABEL
   IR-FIXTURE:BND [: LIFE-BODY ;] IR-CTX:WITH-CONTEXT
   TFALSE TTRUE ;

\ ---- refusals ----------------------------------------------------------------
\ A released table: the store is retired, so every reader is named rather than
\ reading a retired arena.
: RELEASED-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 0 IR-FIXTURE:MODULE-OF {: m:IR-BUILD:module :}
   c m IR-CANON:CANON {: t:IR-CANON:table :}
   t IR-CANON:RELEASE
   t IR-CANON:CELLS drop ;

: RELEASED-RUN ( -- )
   IR-FIXTURE:BND [: RELEASED-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A stream index at the length itself.
: BOUND-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 0 IR-FIXTURE:MODULE-OF {: m:IR-BUILD:module :}
   c m IR-CANON:CANON {: t:IR-CANON:table :}
   t  t IR-CANON:CELLS  IR-CANON:CELL@ drop ;

: BOUND-RUN ( -- )
   IR-FIXTURE:BND [: BOUND-BODY ;] IR-CTX:WITH-CONTEXT ;

\ An identity another module minted. Two modules of one context have different
\ module keys, so the second module's symbol zero is not a row of the first
\ module's canonical table.
: OWNER-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 0 IR-FIXTURE:MODULE-OF {: ma:IR-BUILD:module :}
   c 0 0 IR-FIXTURE:MODULE-OF {: mb:IR-BUILD:module :}
   c ma IR-CANON:CANON {: t:IR-CANON:table :}
   t  mb IR-BUILD:FKEY 0 IR-ID:PACK-SYMBOL  IR-CANON:SYMBOL-ORD drop ;

: OWNER-RUN ( -- )
   IR-FIXTURE:BND [: OWNER-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A name longer than the committed working set. The refusal runs before the
\ canonical store is created, so it costs the context nothing.
create BIG-NAME 300 allot

: BIG-NAME-FILL ( -- )
   300 0 ?do
      $61 BIG-NAME i + c!
   loop ;

: CAP-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c IR-FIXTURE:MK {: b:IR-BUILD:builder :}
   c b 0 0 IR-FIXTURE:BUILD
   BIG-NAME-FILL
   c b BIG-NAME 300 IR-BUILD:INTERN-SYMBOL drop
   c b IR-BUILD:FREEZE {: m:IR-BUILD:module :}
   c m IR-CANON:CANON drop ;

: CAP-RUN ( -- )
   IR-FIXTURE:BND [: CAP-BODY ;] IR-CTX:WITH-CONTEXT ;

\ A module whose own context has torn down. Its tables are unmapped, so it is no
\ longer a frozen module anything may read.
: INNER-MODULE ( IR-CTX:ctx -- IR-BUILD:module )
   0 0 IR-FIXTURE:MODULE-OF ;

: STALE-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   IR-FIXTURE:BND [: INNER-MODULE ;] IR-CTX:WITH-CONTEXT {: m:IR-BUILD:module :}
   c m IR-CANON:CANON drop ;

: STALE-RUN ( -- )
   IR-FIXTURE:BND [: STALE-BODY ;] IR-CTX:WITH-CONTEXT ;

\ One canonical table more than the registry holds.
: CANON-DROP ( IR-CTX:ctx IR-BUILD:module -- )
   IR-CANON:CANON drop ;

: SLOTS-BODY ( IR-CTX:ctx -- )
   {: c:IR-CTX:ctx :}
   c 0 0 IR-FIXTURE:MODULE-OF {: m:IR-BUILD:module :}
   9 0 ?do
      c m CANON-DROP
   loop ;

: SLOTS-RUN ( -- )
   IR-FIXTURE:BND [: SLOTS-BODY ;] IR-CTX:WITH-CONTEXT ;

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
   IR-FIXTURE:BND [: drop ORDER-CASE ;] IR-CTX:WITH-CONTEXT
   IR-FIXTURE:BND [: drop ATTR-ORDER-CASE ;] IR-CTX:WITH-CONTEXT
   IR-FIXTURE:BND [: drop EQUIV-CASE ;] IR-CTX:WITH-CONTEXT
   IR-FIXTURE:BND [: drop SWAP-CASE ;] IR-CTX:WITH-CONTEXT
   IR-FIXTURE:BND [: drop MERGE-CASE ;] IR-CTX:WITH-CONTEXT
   IR-FIXTURE:BND [: drop LIFE-CASE ;] IR-CTX:WITH-CONTEXT
   IR-FIXTURE:BND [: drop RELEASED-CASE ;] IR-CTX:WITH-CONTEXT
   IR-FIXTURE:BND [: drop BOUND-CASE ;] IR-CTX:WITH-CONTEXT
   IR-FIXTURE:BND [: drop OWNER-CASE ;] IR-CTX:WITH-CONTEXT
   IR-FIXTURE:BND [: drop CAP-CASE ;] IR-CTX:WITH-CONTEXT
   IR-FIXTURE:BND [: drop STALE-CASE ;] IR-CTX:WITH-CONTEXT
   IR-FIXTURE:BND [: drop SLOTS-CASE ;] IR-CTX:WITH-CONTEXT
   T-REPORT ;

;package

IR-CANON-TEST:RUN

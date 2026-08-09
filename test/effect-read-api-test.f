\ effect-read-api-test.f - checker effect-read export API + negative regression
\ (dot habu-expose-checker-effect-95e853eb).
\
\ Proves a cold-prefix file (this one, loaded by bin/hb) resolves the checker's
\ minimal effect-read export API and reads a certified word's din/dout arity +
\ per-position family class. The API (src/core/checker.f) is:
\   EFFECT-QUERY   ( ptr u8 n -- bool )   resolve NAME's active effect into query state
\   EFFECT-DIN-N   ( -- n )               fixed din term count of the queried effect
\   EFFECT-DOUT-N  ( -- n )               fixed dout term count
\   EFFECT-DIN-FAM ( i -- fam )           EFAM-* family of din term i (top = 0)
\   EFFECT-DOUT-FAM ( i -- fam )          EFAM-* family of dout term i (top = 0)
\   EFFECT-DIN-CELLS ( -- n )             fixed din width in STACK CELLS, or CELLS-NONE
\   EFFECT-DOUT-CELLS ( -- n )            fixed dout width in cells, or CELLS-NONE
\   EFFECT-DIN-SLOT ( i -- n )            bundle slot+1 of din term i, 0 = logical
\   EFFECT-DOUT-SLOT ( i -- n )           bundle slot+1 of dout term i, 0 = logical
\ Family ABI (EFAM-*, mirrored below as ERA-* to pin the numeric contract):
\   0 gray (var/row/atom/param)   1 scalar (con)   2 pointer (ptr)   3 xt (quot)
\
\ Only EFFECT-QUERY is trusted; the readers are checked. Every entry is name-stripped
\ past the seal, so it is uncallable from checked code and from bare interpret - the
\ prefix consumers (the top-row hook) reach it as compiled calls from an unchecked
\ boundary. This test does the same: the assertion words below run inside a single
\ `0 set-check` window. Fixtures and queries are GLOBAL: EFFECT-QUERY resolves a NAME
\ against the active package context, and at top level no package is active.
\
\ NEGATIVE REGRESSION (surface pin): the assertion words call each API word by name.
\ Renaming or removing an EFFECT-* entry in checker.f makes that name undefined at
\ load -> `hb: ... undefined word` + rc 70, so this test fails LOUDLY. The scalar
\ family assertions additionally pin the ABI values: a changed family projection or
\ arity flips a T= and fails the run.
\
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f test/effect-read-api-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/adt/option.f

0 constant ERA-GRAY     1 constant ERA-SCALAR
2 constant ERA-POINTER  3 constant ERA-XT

\ Certified fixtures (global, checked) with known din/dout family rows.
: ERA-A ( n -- n n ) dup ;                          \ scalar in; two scalars out
: ERA-G ( a -- a a ) dup ;                          \ polymorphic var (gray) in/out
: ERA-P ( ptr u8 n -- n ) drop drop 42 ;            \ pointer + scalar in; scalar out

\ THE PAIR THE SLOT READER EXISTS FOR, and the reason it has to be a pair. These
\ two signatures agree on every other thing the API reports - three din terms,
\ three din cells, and the same family for each of the three - so an assertion
\ about either one alone would pass just as well against a reader that answered a
\ constant. They differ in one respect only: the first names a value that occupies
\ two of those three cells, and the second names two values that occupy one cell
\ each. Renaming the second cell-wise is correct; renaming the first cell-wise
\ takes a value apart.
\
\ These three carry an owning package, unlike the three fixtures above them. The
\ globals are global because they were written before EFFECT-QUERY was known to
\ resolve a qualified name; it does, so nothing new here needs to be global, and
\ the assertions below name them the way the rest of the tree names a package
\ word.
package ERA-FIX
public

: BUNDLE ( option<n> n -- n option<n> ) swap ;
: TWOVAR ( a b n -- n a b ) swap ;
: MKOPT  ( n -- option<n> ) OPTION:SOME ;           \ a user word whose OUTPUT is bundled

;package

\ The effect-read API reads raw effect-store state, so - like the top-row hook - it
\ is called from an unchecked window. `0 set-check` opens the named boundary; T= /
\ TTRUE / T-LABEL from lib/test.f stay callable across it.
0 set-check

: ERA-SCALAR-PRODUCER ( -- )
   s" a net-scalar word: din 1 scalar, dout 2 scalars" T-LABEL
   s" ERA-A" EFFECT-QUERY TTRUE
   EFFECT-DIN-N 1 T=            EFFECT-DOUT-N 2 T=
   0 EFFECT-DIN-FAM ERA-SCALAR T=
   0 EFFECT-DOUT-FAM ERA-SCALAR T=  1 EFFECT-DOUT-FAM ERA-SCALAR T= ;

: ERA-POLYMORPHIC ( -- )
   s" a row-polymorphic var word: din/dout families are gray" T-LABEL
   s" ERA-G" EFFECT-QUERY TTRUE
   EFFECT-DIN-N 1 T=            EFFECT-DOUT-N 2 T=
   0 EFFECT-DIN-FAM ERA-GRAY T=
   0 EFFECT-DOUT-FAM ERA-GRAY T= ;

: ERA-POINTER-ROW ( -- )
   s" a pointer+scalar consumer: din0 scalar (top), din1 pointer" T-LABEL
   s" ERA-P" EFFECT-QUERY TTRUE
   EFFECT-DIN-N 2 T=            EFFECT-DOUT-N 1 T=
   0 EFFECT-DIN-FAM ERA-SCALAR T=
   1 EFFECT-DIN-FAM ERA-POINTER T=
   0 EFFECT-DOUT-FAM ERA-SCALAR T= ;

: ERA-PRIM-ROW ( -- )
   s" prim type ( ptr u8 n -- ): pure consumer, din1 pointer, dout empty" T-LABEL
   s" type" EFFECT-QUERY TTRUE
   EFFECT-DOUT-N 0 T=
   1 EFFECT-DIN-FAM ERA-POINTER T=
   s" @ ( ptr a -- a ): din0 pointer, dout1 gray" T-LABEL
   s" @" EFFECT-QUERY TTRUE
   0 EFFECT-DIN-FAM ERA-POINTER T=
   0 EFFECT-DOUT-FAM ERA-GRAY T= ;

: ERA-EDGES ( -- )
   s" an unknown word does not resolve" T-LABEL
   s" ZZ-NO-SUCH-WORD" EFFECT-QUERY TFALSE
   s" an out-of-range din index reads gray" T-LABEL
   s" ERA-A" EFFECT-QUERY TTRUE
   5 EFFECT-DIN-FAM ERA-GRAY T=
   9 EFFECT-DOUT-FAM ERA-GRAY T= ;

\ ---- the widths, which are a different number from the counts -----------------
\ EFFECT-DIN-CELLS / EFFECT-DOUT-CELLS answer a row's width in STACK CELLS, the
\ number a call site moves (dot habu-export-the-checker-2bbc831c). The readers
\ above answer TERMS, and the two are equal only while every term is one cell
\ wide - which is true of the fixtures here and is exactly why the pair below
\ cannot be proved from these three alone. ERA-G is the case that matters most:
\ its terms are gray, the family enum says nothing about their width, and the
\ width is stated anyway, because it comes from the checker's own ROW-TERM-CELLS
\ rather than from the enum.
\ These are this file's only NEW words, so unlike the global fixtures above -
\ which must be global for EFFECT-QUERY to resolve them by bare name from top
\ level - they carry an owning package like any other module surface.
package ERA-WIDTH
private

: ERA-CELLS ( -- )
   s" a scalar word's rows are as many cells as terms" T-LABEL
   s" ERA-A" EFFECT-QUERY TTRUE
   EFFECT-DIN-CELLS 1 T=        EFFECT-DOUT-CELLS 2 T=

   s" and a GRAY row is sized too, which the family enum could not do" T-LABEL
   s" ERA-G" EFFECT-QUERY TTRUE
   0 EFFECT-DIN-FAM ERA-GRAY T=
   EFFECT-DIN-CELLS 1 T=        EFFECT-DOUT-CELLS 2 T=

   s" a pointer row: ptr u8 n is two terms and two cells" T-LABEL
   s" ERA-P" EFFECT-QUERY TTRUE
   EFFECT-DIN-CELLS 2 T=        EFFECT-DOUT-CELLS 1 T=

   s" an empty row is zero cells, not absent" T-LABEL
   s" type" EFFECT-QUERY TTRUE
   EFFECT-DOUT-CELLS 0 T= ;

\ ---- which cell of a value each term is, which neither of the other two says --
\ EFFECT-DIN-SLOT / EFFECT-DOUT-SLOT report a term's position inside a multi-cell
\ value as slot+1, and 0 for an ordinary term (dot habu-rename-over-rows-982167af).
\ The two fixtures are built to fool a reader that guesses: they agree on the term
\ count, the cell count and every family, so only a reader that reaches the stored
\ marker can tell them apart. The negative half is asserted as loudly as the
\ positive one - a reader that answered "bundled" everywhere would pass the first
\ block and fail the second.
: ERA-SLOTS ( -- )
   s" a bundled signature and a two-variable one agree on counts and families" T-LABEL
   s" ERA-FIX:BUNDLE" EFFECT-QUERY TTRUE
   EFFECT-DIN-N 3 T=            EFFECT-DIN-CELLS 3 T=
   0 EFFECT-DIN-FAM ERA-SCALAR T=
   1 EFFECT-DIN-FAM ERA-GRAY T=   2 EFFECT-DIN-FAM ERA-GRAY T=
   s" ERA-FIX:TWOVAR" EFFECT-QUERY TTRUE
   EFFECT-DIN-N 3 T=            EFFECT-DIN-CELLS 3 T=
   0 EFFECT-DIN-FAM ERA-SCALAR T=
   1 EFFECT-DIN-FAM ERA-GRAY T=   2 EFFECT-DIN-FAM ERA-GRAY T=

   s" and the slots separate them: the bundle's two cells carry their positions" T-LABEL
   s" ERA-FIX:BUNDLE" EFFECT-QUERY TTRUE
   0 EFFECT-DIN-SLOT 0 T=       \ the plain n on top
   1 EFFECT-DIN-SLOT 2 T=       \ the value's upper cell, slot 1
   2 EFFECT-DIN-SLOT 1 T=       \ its lower cell, slot 0
   s" while two independent variables are all logical" T-LABEL
   s" ERA-FIX:TWOVAR" EFFECT-QUERY TTRUE
   0 EFFECT-DIN-SLOT 0 T=  1 EFFECT-DIN-SLOT 0 T=  2 EFFECT-DIN-SLOT 0 T=

   s" the output side reports it too, for a user word returning the value" T-LABEL
   s" ERA-FIX:MKOPT" EFFECT-QUERY TTRUE
   EFFECT-DOUT-N 2 T=           EFFECT-DOUT-CELLS 2 T=
   0 EFFECT-DOUT-SLOT 2 T=      1 EFFECT-DOUT-SLOT 1 T=

   s" a generated constructor keeps it as one wide term instead, so no slot marks it" T-LABEL
   s" OPTION:SOME" EFFECT-QUERY TTRUE
   EFFECT-DOUT-N 1 T=           EFFECT-DOUT-CELLS 2 T=
   0 EFFECT-DOUT-SLOT 0 T=

   s" an ordinary word has no bundled cell on either side" T-LABEL
   s" ERA-A" EFFECT-QUERY TTRUE
   0 EFFECT-DIN-SLOT 0 T=
   0 EFFECT-DOUT-SLOT 0 T=      1 EFFECT-DOUT-SLOT 0 T=

   s" and an out-of-range index reads logical rather than running off the row" T-LABEL
   9 EFFECT-DIN-SLOT 0 T=       9 EFFECT-DOUT-SLOT 0 T= ;

\ ---- the width against the checker's own answer, over the whole dictionary ----
\ WHAT THIS PROVES THAT THE FIXTURES ABOVE CANNOT. The checker computes a row's
\ width twice, by two walks over two different representations. ROW-CELLS walks
\ the LIVE terms (R-RES / P>TYPE / P>REST) as an effect is recorded, and its din
\ answer is kept in the record as ER.MINI, which SIG-MIN-IN reads back.
\ EFFECT-DIN-CELLS sums the per-cell widths stored in the EN-node graph, walking
\ EN.B down the stored row. Neither is derived from the other at run time. So
\ demanding they agree for EVERY record the engine holds is a real measurement:
\ an export that mis-sizes a row, or a row walk that runs off the wrong field,
\ or a stored width written on the wrong node, all disagree here. A hand-written
\ fixture could not reach the layout-family rows the engine itself carries.
\
\ Nothing here fixes the POPULATION - the count is whatever the loaded engine
\ holds - so the assertion is that disagreements are zero and that the population
\ was not empty, which is what stops a walk that silently examined nothing from
\ passing.
variable ERA-AGREE   variable ERA-DIFF

: ERA-ONE ( ptr u8 n -- )
   {: a u:n :} \ typed-local-lint: allow-bare-local - a keeps the ptr u8 byte-span role
   a u EFFECT-QUERY 0= if exit then
   a u SIG-MIN-IN {: mini:n :}
   EFFECT-DIN-CELLS mini = if ERA-AGREE @ 1+ ERA-AGREE ! exit then
   ERA-DIFF @ 1+ ERA-DIFF ! ;

: ERA-WALK ( -- )
   0 ERA-AGREE !  0 ERA-DIFF !
   0 begin dup ndict@ < while
      dup XREF-REC dup XREF-RETIRED? if drop else XREF-NAME$ ERA-ONE then
      1+
   repeat drop ;

: ERA-AGREEMENT ( -- )
   ERA-WALK
   s" every certified record's din width equals the min-in the checker recorded" T-LABEL
   ERA-DIFF @ 0 T=
   s" and the walk really did examine the engine's certified records" T-LABEL
   ERA-AGREE @ 100 > TTRUE ;

public

: MAIN ( -- )
   T-RESET
   ERA-CELLS
   ERA-SLOTS
   ERA-AGREEMENT
   T-REPORT
   s" effect-read-api widths: ok" type cr ;

;package

: ERA-MAIN ( -- )
   T-RESET
   ERA-SCALAR-PRODUCER
   ERA-POLYMORPHIC
   ERA-POINTER-ROW
   ERA-PRIM-ROW
   ERA-EDGES
   T-REPORT
   s" effect-read-api: ok" type cr ;

ERA-MAIN
ERA-WIDTH:MAIN

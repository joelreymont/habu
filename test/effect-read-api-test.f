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
\   EFFECT-DIN-QUOT ( i -- bool )         latch onto din term i's own quotation rows
\   EFFECT-DOUT-QUOT ( i -- bool )        the same for a dout term
\   EFFECT-QUOT-UP ( -- bool )            put the displaced rows back
\   EFFECT-QUOT-SIMPLE? ( -- bool )       is the latched quotation an ordinary routine?
\   EFFECT-RET-NEUTRAL? ( -- bool )       does the queried WORD leave the return stack alone?
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

\ THE PAIR THE QUOTATION DESCENT EXISTS FOR, built on the same argument as the
\ bundle pair above: everything the OLDER readers report about these two is
\ identical - three din terms, three din cells, and the families xt, scalar,
\ scalar in that order - so an assertion about either one alone would pass
\ against a reader that answered a constant, and an assertion about both would
\ pass against a reader that read the OUTER row and called it the quotation's.
\ They differ in one respect only, and it is the one thing no other reader can
\ see: the quotation the first takes consumes two cells and leaves one, and the
\ quotation the second takes consumes one and leaves two.
: QTAKE2 ( n n [ n n -- n ] -- n )
   {: a:n b:n q :} \ typed-local-lint: allow-bare-local - q keeps the quotation's own effect
   a b q execute ;

: QTAKE1 ( n n [ n -- n n ] -- n n )
   {: a:n b:n q :} \ typed-local-lint: allow-bare-local - q keeps the quotation's own effect
   a drop b q execute ;

\ Three din terms and three cells again, and not a quotation anywhere: the
\ descent has to answer false rather than walk a node of another shape.
: QNONE ( n n n -- n )
   {: a:n b:n c:n :}
   a b + c + ;

\ A quotation on the OUTPUT side, which is the shape a word that hands its caller
\ a body has. Its own effect is a third, distinct arity, so a reader that
\ answered the din row's quotation here would be caught.
: QSUM3 ( n n n -- n )
   {: a:n b:n c:n :}
   a b + c + ;

: QGIVE ( -- [ n n n -- n ] )
   [: QSUM3 ;] ;

\ A quotation that also states a RETURN effect. Its data rows are the same two
\ cells in and one out as QTAKE2's, so nothing about its arity separates them;
\ what separates them is that this one's return rows are two different rows, and
\ a body whose return stack does not come back the way it arrived is not one a
\ caller may reach with an ordinary branch.
: QRET ( n [ n -- n | a -- a ] -- n )
   {: a:n q :} \ typed-local-lint: allow-bare-local - q keeps the quotation's own effect
   a ;

\ ---- the four words EFFECT-RET-NEUTRAL? is asked about -----------------------
\ THE MIDDLE TWO ARE A PAIR AND THAT IS THE WHOLE FIXTURE. Both of them WRITE a
\ `| rin -- rout` clause, so a reader that answered from the clause's presence -
\ from ER.HASR, the obvious thing to expose - gives both the same answer and one
\ of the two assertions below reds. They differ in what the rows SAY: RETVAR
\ names one row variable on both sides and therefore moves nothing, while
\ RETPUSH names an empty row in and a one-term row out and therefore leaves a
\ cell on its caller's return stack. Neutrality is a fact about the rows; the
\ clause is a fact about the spelling.
\
\ RETNONE is the ordinary case that must not be collateral damage - almost every
\ word in the tree is shaped like it - and RETPOP is RETPUSH's mirror, so a
\ reader that happened to look at only one of the two rows is caught by whichever
\ side it ignored.
: RETNONE ( n -- n ) ;                              \ no clause at all: neutral

\ The case the first consumer actually meets: a word with no clause that USES the
\ return stack inside itself. It is neutral to its CALLER - which is the only
\ thing neutrality claims - and a reader that answered "this word mentions >r"
\ rather than "this word's rows move nothing" would get it wrong and refuse
\ every internal use in the tree.
: RETBAL  ( n -- n ) >r r> ;                        \ balanced internal use: still neutral

: RETVAR  ( n | R -- n | R ) ;                      \ a clause that moves nothing: neutral

: RETPUSH ( n | -- | n ) >r ;                       \ leaves a cell on the caller's return stack

: RETPOP  ( | n -- n | ) r> ;                       \ takes one off it

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

\ ---- what a quotation term takes and leaves, which is a whole effect ----------
\ EFFECT-DIN-QUOT / EFFECT-DOUT-QUOT move the query latch onto the rows of the
\ quotation a term IS, EFFECT-QUOT-UP moves it back, and EFFECT-QUOT-SIMPLE? says
\ whether that quotation is one a caller may compile as an ordinary routine
\ (dot habu-export-a-certified-f5a7561d). The pair below is built the way the
\ slot pair above is: the two fixtures agree on every number the OLDER readers
\ report and differ only inside the quotation, so nothing here can be answered by
\ a reader that never left the outer row.
package ERA-QUOT
private

\ The three numbers the older readers give for both fixtures of the pair. Asked
\ of each of them, so "they agree" is measured rather than asserted once.
: OUTER ( -- )
   EFFECT-DIN-N 3 T=            EFFECT-DIN-CELLS 3 T=
   0 EFFECT-DIN-FAM ERA-XT T=
   1 EFFECT-DIN-FAM ERA-SCALAR T=
   2 EFFECT-DIN-FAM ERA-SCALAR T= ;

: PAIR ( -- )
   s" two quotation-taking words agree on every number the older readers give" T-LABEL
   s" ERA-FIX:QTAKE2" EFFECT-QUERY TTRUE   OUTER
   s" ERA-FIX:QTAKE1" EFFECT-QUERY TTRUE   OUTER

   s" and the descent separates them: one quotation takes two and leaves one" T-LABEL
   s" ERA-FIX:QTAKE2" EFFECT-QUERY TTRUE
   0 EFFECT-DIN-QUOT TTRUE
   EFFECT-DIN-N 2 T=            EFFECT-DOUT-N 1 T=
   EFFECT-DIN-CELLS 2 T=        EFFECT-DOUT-CELLS 1 T=

   s" while the other takes one and leaves two" T-LABEL
   s" ERA-FIX:QTAKE1" EFFECT-QUERY TTRUE
   0 EFFECT-DIN-QUOT TTRUE
   EFFECT-DIN-N 1 T=            EFFECT-DOUT-N 2 T=
   EFFECT-DIN-CELLS 1 T=        EFFECT-DOUT-CELLS 2 T= ;

\ THE INDEX HAS TO DECIDE, not the row. Both refusals below are asked of a row
\ that DOES hold a quotation at another position, so a reader that answered "this
\ row has one somewhere" would pass the QNONE case and fail here.
: REFUSALS ( -- )
   s" a scalar term of a row that also holds a quotation is not one" T-LABEL
   s" ERA-FIX:QTAKE2" EFFECT-QUERY TTRUE
   1 EFFECT-DIN-QUOT TFALSE
   2 EFFECT-DIN-QUOT TFALSE
   s" and the refused descent left the latch on the outer row" T-LABEL
   OUTER

   s" an index past the end of the row is not one either" T-LABEL
   9 EFFECT-DIN-QUOT TFALSE

   s" a row with no quotation in it at all has none at position zero" T-LABEL
   s" ERA-FIX:QNONE" EFFECT-QUERY TTRUE
   EFFECT-DIN-N 3 T=
   0 EFFECT-DIN-QUOT TFALSE

   s" and the output side is asked separately from the input side" T-LABEL
   s" ERA-FIX:QTAKE2" EFFECT-QUERY TTRUE
   0 EFFECT-DOUT-QUOT TFALSE ;

: GIVEN ( -- )
   s" a word that hands its caller a body carries the quotation on the OUT side" T-LABEL
   s" ERA-FIX:QGIVE" EFFECT-QUERY TTRUE
   EFFECT-DIN-N 0 T=            EFFECT-DOUT-N 1 T=
   0 EFFECT-DOUT-FAM ERA-XT T=
   0 EFFECT-DOUT-QUOT TTRUE
   EFFECT-DIN-N 3 T=            EFFECT-DOUT-N 1 T=
   EFFECT-DIN-CELLS 3 T=        EFFECT-DOUT-CELLS 1 T= ;

\ THE LATCH IS ONE PAIR OF CELLS, so the whole value of the descent depends on it
\ going back. The outer numbers are read, the descent is made and read, the latch
\ is put back, and the outer numbers are read AGAIN - without resolving the name
\ a second time, which is the point: a consumer walking a body cannot afford to
\ re-resolve, and a save that did not restore would show up here as the
\ quotation's numbers where the word's belong.
: RESTORE ( -- )
   s" the latch goes back to the outer row, with no second query" T-LABEL
   s" ERA-FIX:QGIVE" EFFECT-QUERY TTRUE
   EFFECT-DOUT-N 1 T=
   0 EFFECT-DOUT-QUOT TTRUE
   EFFECT-DIN-N 3 T=
   EFFECT-QUOT-UP TTRUE
   EFFECT-DIN-N 0 T=            EFFECT-DOUT-N 1 T=
   0 EFFECT-DOUT-FAM ERA-XT T=

   s" a second descent while one is open is refused, and changes nothing" T-LABEL
   s" ERA-FIX:QTAKE2" EFFECT-QUERY TTRUE
   0 EFFECT-DIN-QUOT TTRUE
   EFFECT-DIN-N 2 T=
   0 EFFECT-DIN-QUOT TFALSE
   EFFECT-DIN-N 2 T=
   EFFECT-QUOT-UP TTRUE
   OUTER

   s" and putting back a latch nothing displaced is refused too" T-LABEL
   EFFECT-QUOT-UP TFALSE
   OUTER

   s" a fresh query closes an open descent rather than carrying it over" T-LABEL
   s" ERA-FIX:QTAKE2" EFFECT-QUERY TTRUE
   0 EFFECT-DIN-QUOT TTRUE
   s" ERA-A" EFFECT-QUERY TTRUE
   EFFECT-QUOT-SIMPLE? TFALSE
   EFFECT-QUOT-UP TFALSE
   EFFECT-DIN-N 1 T=            EFFECT-DOUT-N 2 T= ;

\ IS IT A BODY A CALLER MAY COMPILE? The two quotations below have the SAME data
\ rows - one cell in, one cell out - so their arity separates nothing; what
\ separates them is the return clause the second one states, and a body whose
\ return stack does not come back the way it arrived is not one an ordinary
\ branch reaches.
\
\ THE OTHER TWO CLAUSES ARE PROVED BY MUTATING THE CHECKER, because no DECLARED
\ signature can reach them. MK-QUOT (src/core/checker.f) writes zero into both the
\ throw-edge and the dead-fall-through cells, and only the checker's own inference
\ of a quotation BODY ever writes anything else - so every quotation term any
\ stored record holds today carries zero in both, and a fixture written here could
\ not carry anything else. That is a fact about where the two cells come FROM, not
\ a gap in the rule: the reader exists for a consumer that will ask about inferred
\ quotations, and a consumer that emitted code after a call to a body which never
\ returns would be writing unreachable code it believed in.
\
\ TRANSCRIPT, on this tree, 2026-08-10. Each mutation is made at the RECORD-STORE
\ site (the E-COPY* arm for T-QUOT), not at MK-QUOT, so the checker's own live
\ inference is untouched and the boot prefix still certifies - mutating MK-QUOT
\ instead makes every quotation in the tree never-returning and the engine stops
\ loading at `call-participant`, which proves the field matters to the CHECKER and
\ says nothing about this reader.
\   storing 1 into EN.E of every recorded quotation term turns
\     "a quotation with neither a throw edge nor a dead fall-through is simple"
\     red (assert 82, expected true got false) and moves nothing else;
\   storing 1 into EN.F instead turns the same case red on its own;
\   deleting the neutral-return clause from EFFECT-QUOT-SIMPLE? turns
\     "a quotation that states a return effect is not" red (assert 88).
\ So all three clauses are live and each is load-bearing by itself. Reverted after
\ each run; the two fixtures below are the declared-signature half.
: SIMPLE ( -- )
   s" a quotation with neither a throw edge nor a dead fall-through is simple" T-LABEL
   s" ERA-FIX:QTAKE2" EFFECT-QUERY TTRUE
   0 EFFECT-DIN-QUOT TTRUE
   EFFECT-QUOT-SIMPLE? TTRUE
   EFFECT-QUOT-UP TTRUE

   s" a quotation that states a return effect is not, though its arity is ordinary" T-LABEL
   s" ERA-FIX:QRET" EFFECT-QUERY TTRUE
   0 EFFECT-DIN-QUOT TTRUE
   EFFECT-DIN-N 1 T=            EFFECT-DOUT-N 1 T=
   EFFECT-QUOT-SIMPLE? TFALSE
   EFFECT-QUOT-UP TTRUE

   s" and with no descent open there is no quotation to call simple" T-LABEL
   EFFECT-QUOT-SIMPLE? TFALSE ;

public

: MAIN ( -- )
   T-RESET
   PAIR
   REFUSALS
   GIVEN
   RESTORE
   SIMPLE
   T-REPORT
   s" effect-read-api quotations: ok" type cr ;

;package

\ ---- does the queried WORD leave its caller's return stack alone? -------------
\ EFFECT-RET-NEUTRAL? ( -- bool ), the reader a compiler that models the return
\ stack at COMPILE time has to consult before it may compile a call. Its first
\ consumer is the native chain's return-stack lowering
\ (src/compiler/native/dict.f), which has no cell to put a call's return-stack
\ motion in and refuses any callee that has one.
\
\ WHY THE FIXTURES ARE A PAIR RATHER THAN A LIST. RETVAR and RETPUSH both carry a
\ `| rin -- rout` clause, so the two assertions below disagree only if the reader
\ reads the ROWS; a reader keyed on the clause's presence answers them the same
\ way and reds one of them whichever way it answers. That is the mutation this
\ file cannot make for itself - the clause flag is not exposed - so the pair
\ stands in for it, and it is why exposing ER.HASR instead would have been the
\ wrong export rather than merely a coarser one.
\
\ TRANSCRIPT, on this tree, 2026-08-13. Each mutation is made at
\ EFFECT-RET-NEUTRAL? in src/core/checker.f, rebuilt with `install --force`, and
\ reverted after the run. Every clause reds a DIFFERENT assertion, so none of
\ them is carried by another:
\   returning `EFFQ-RIN @ 0= EFFQ-ROUT @ 0= and` alone - which is the reader that
\     exposing ER.HASR would have given, neutrality keyed on whether a clause was
\     written - reds assert 4, RETVAR, and nothing else;
\   deleting the absent-rows clause, so a word with no clause falls through to
\     the row comparison, reds asserts 2 and 6 (RETNONE, RETBAL);
\   returning true whenever a name resolved reds asserts 8 and 10
\     (RETPUSH, RETPOP);
\   deleting the EFFQ-OK guard reds assert 18 (the unresolved name).
package ERA-RET

private

: NEUTRAL ( -- )
   s" a word with no return clause leaves the return stack alone" T-LABEL
   s" ERA-FIX:RETNONE" EFFECT-QUERY TTRUE
   EFFECT-RET-NEUTRAL? TTRUE

   s" a clause naming one row variable on both sides also moves nothing" T-LABEL
   s" ERA-FIX:RETVAR" EFFECT-QUERY TTRUE
   EFFECT-RET-NEUTRAL? TTRUE

   s" a balanced internal >r/r> is invisible to the caller" T-LABEL
   s" ERA-FIX:RETBAL" EFFECT-QUERY TTRUE
   EFFECT-RET-NEUTRAL? TTRUE ;

: MOVING ( -- )
   s" a word that leaves a cell on the caller's return stack is not neutral" T-LABEL
   s" ERA-FIX:RETPUSH" EFFECT-QUERY TTRUE
   EFFECT-RET-NEUTRAL? TFALSE

   s" nor is one that takes a cell off it" T-LABEL
   s" ERA-FIX:RETPOP" EFFECT-QUERY TTRUE
   EFFECT-RET-NEUTRAL? TFALSE ;

\ The data rows say nothing about this, which is what makes the reader worth
\ having: RETPUSH and RETPOP disagree with RETNONE about the return stack while
\ RETVAR agrees with it, and no din/dout reader can tell RETVAR from RETPUSH's
\ neighbourhood - so these assertions pin the return rows specifically.
: NOT-THE-DATA-ROWS ( -- )
   s" the data rows do not answer the return question" T-LABEL
   s" ERA-FIX:RETVAR" EFFECT-QUERY TTRUE
   EFFECT-DIN-N 1 T=            EFFECT-DOUT-N 1 T=
   s" ERA-FIX:RETNONE" EFFECT-QUERY TTRUE
   EFFECT-DIN-N 1 T=            EFFECT-DOUT-N 1 T= ;

\ Nothing resolved means nothing promised. It is the same fail-closed direction
\ EFFECT-QUOT-SIMPLE? takes with no descent open: answering true here would let a
\ caller compile an unresolved name as a neutral one.
: UNRESOLVED ( -- )
   s" an unknown word is not neutral, because it is not anything" T-LABEL
   s" ZZ-NO-SUCH-WORD" EFFECT-QUERY TFALSE
   EFFECT-RET-NEUTRAL? TFALSE ;

public

: MAIN ( -- )
   T-RESET
   NEUTRAL
   MOVING
   NOT-THE-DATA-ROWS
   UNRESOLVED
   T-REPORT
   s" effect-read-api return rows: ok" type cr ;

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
ERA-QUOT:MAIN
ERA-RET:MAIN

\ ---- what a catch site instantiated, outside any recording --------------------
\ EFFECT-CATCH-CELLS ( n -- n n ), the reader a compiler that turns a catch site
\ into a call has to consult for the window that call moves. Unlike every reader
\ above it is not asked about a NAME: `catch` has no declared effect, its rows are
\ built per site, and the answer is filed under the token the site stands on while
\ a source-tape observer is recording. Its first consumer is the native chain
\ (src/compiler/native/dict.f CATCH-CELLS), and the recorded answers are measured
\ where a recording exists, in test/compiler/native-catch.f.
\
\ WHAT IS MEASURABLE HERE IS THE OTHER HALF, and it is the half a consumer's
\ safety rests on: with no unit recording, the ordinal is an ordinal into nothing
\ and every token answers absent. That is the same fail-closed direction
\ EFFECT-RET-NEUTRAL? takes with no name resolved - a width answered here would
\ let a caller compile a call against a number nobody proved.
package ERA-CATCH

private

: NO-RECORDING ( -- )
   s" with no unit recording, no token carries a window" T-LABEL
   0 EFFECT-CATCH-CELLS  CELLS-NONE T=  CELLS-NONE T=
   1 EFFECT-CATCH-CELLS  CELLS-NONE T=  CELLS-NONE T=
   99 EFFECT-CATCH-CELLS CELLS-NONE T=  CELLS-NONE T=

   s" and a negative ordinal is no more recorded than any other" T-LABEL
   -1 EFFECT-CATCH-CELLS CELLS-NONE T=  CELLS-NONE T= ;

public

: MAIN ( -- )
   T-RESET
   NO-RECORDING
   T-REPORT
   s" effect-read-api catch windows: ok" type cr ;

;package

ERA-CATCH:MAIN

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

0 constant ERA-GRAY     1 constant ERA-SCALAR
2 constant ERA-POINTER  3 constant ERA-XT

\ Certified fixtures (global, checked) with known din/dout family rows.
: ERA-A ( n -- n n ) dup ;                          \ scalar in; two scalars out
: ERA-G ( a -- a a ) dup ;                          \ polymorphic var (gray) in/out
: ERA-P ( ptr u8 n -- n ) drop drop 42 ;            \ pointer + scalar in; scalar out

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

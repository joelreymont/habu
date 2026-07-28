\ raw-storage-load-seal-test.f - the raw-storage nominal seal on the NATIVE
\ `bin/hb --load` definer path (dot habu-seal-raw-storage-d5871f3f).
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f
\   src/core/pointer-storage.f test/checker-assert.f
\   test/raw-storage-load-seal-test.f
\
\ WHY THIS FILE IS SEPARATE FROM test/pointer-storage-test.f. That suite
\ registers its raw definers by re-driving the source through verify-source,
\ which is the shared pre-verification front end. This suite registers nothing:
\ every cell below is created by the engine itself while this file loads, so the
\ only effect the checker ever sees is the one the native definer published. The
\ two paths used to disagree. verify-source sealed the cells and the native load
\ path did not, so a definition could turn a plain integer into a sealed nominal
\ with no converter, and `bin/hb --load` is the path every tool and gate runs.
\
\ WHAT IS PINNED. Reading back a cell of raw dictionary storage yields a value
\ the checker refuses to bind to a nominal family, whichever definer made the
\ cell: `variable`, `create`, `constant`, `PTR-VARIABLE`, or any user definer
\ built from `create ... does>`. The same cells keep certifying plain scalar
\ round-trips, so the seal removes the forgery and nothing else.
\
\ Verdicts from CHECK-QUIET-CANDIDATE!: -1 certified, 0 refused, 1 unresolvable.

require lib/errors.f
require lib/string.f
require lib/test.f
require src/core/pointer-storage.f
require test/checker-assert.f

package RAW-LOAD-SEAL

\ ---- the nominal family a forged value would have to manufacture -------------
NEWTYPE rlsk 0

\ ---- one cell per raw storage definer, all created by the native engine ------
variable VAR-CELL
create CRE-CELL 8 allot
7 constant CON-CELL
PTR-VARIABLE PTR-CELL

\ A user-written definer: `create ... does>` is the general shape the four
\ definers above are special cases of, and its created word takes the runtime
\ effect declared on the `does>` clause. A free type variable there is the same
\ forgery surface, so the seal has to reach it too.
: CELL-DEFINER ( -- ) create 0 , does> ( -- a ) @ ;
CELL-DEFINER DOES-CELL

\ ---- the forgeries: every one of these must be refused -----------------------
\ Each stores a plain integer (or nothing at all) and reads the cell back as the
\ nominal family. There is no converter anywhere, so certifying any of them
\ means the nominal seal can be bypassed through raw storage.
: FORGE-VAR ( -- )
   s" F1 ( n -- rlsk ) VAR-CELL ! VAR-CELL @" CHECK-QUIET-CANDIDATE! 0 T= ;

: FORGE-CREATE ( -- )
   s" F2 ( n -- rlsk ) CRE-CELL ! CRE-CELL @" CHECK-QUIET-CANDIDATE! 0 T= ;

: FORGE-CONSTANT ( -- )
   s" F3 ( -- rlsk ) CON-CELL" CHECK-QUIET-CANDIDATE! 0 T= ;

: FORGE-PTR ( -- )
   s" F4 ( ptr a -- rlsk ) PTR-CELL ! PTR-CELL @" CHECK-QUIET-CANDIDATE! 0 T= ;

: FORGE-DOES ( -- )
   s" F5 ( -- rlsk ) DOES-CELL" CHECK-QUIET-CANDIDATE! 0 T= ;

\ ---- the seal must not cost the honest uses ----------------------------------
\ A plain scalar round-trip through the very same cells still certifies, so the
\ rejections above are about nominal identity and not about raw storage being
\ unusable.
: SCALAR-VAR ( -- )
   s" S1 ( n -- n ) VAR-CELL ! VAR-CELL @" CHECK-QUIET-CANDIDATE! -1 T= ;

: SCALAR-CREATE ( -- )
   s" S2 ( n -- n ) CRE-CELL ! CRE-CELL @" CHECK-QUIET-CANDIDATE! -1 T= ;

: SCALAR-CONSTANT ( -- )
   s" S3 ( -- n ) CON-CELL" CHECK-QUIET-CANDIDATE! -1 T= ;

: SCALAR-PTR ( -- )
   s" S4 ( -- ptr ptr a ) PTR-CELL" CHECK-QUIET-CANDIDATE! -1 T= ;

: SCALAR-DOES ( -- )
   s" S5 ( -- n ) DOES-CELL" CHECK-QUIET-CANDIDATE! -1 T= ;

\ ---- the cells still work at runtime -----------------------------------------
: ROUND-TRIP ( -- )
   5 VAR-CELL !  VAR-CELL @ 5 T=
   6 CRE-CELL !  CRE-CELL @ 6 T=
   CON-CELL 7 T= ;

: FORGERIES ( -- )
   FORGE-VAR FORGE-CREATE FORGE-CONSTANT FORGE-PTR FORGE-DOES ;

: SCALARS ( -- )
   SCALAR-VAR SCALAR-CREATE SCALAR-CONSTANT SCALAR-PTR SCALAR-DOES ;

: RUN ( -- )
   T-RESET
   FORGERIES
   SCALARS
   ROUND-TRIP
   T-REPORT ;

RUN

;package

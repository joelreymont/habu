\ raw-storage-load-seal-test.f - the raw-storage nominal seal on the native
\ `bin/hb --load` definer path.
\ The engine creates every cell as this file loads, so the checker sees only the
\ native definers' effects (test/pointer-storage-test.f re-drives
\ verify-source). CHECK-QUIET-CANDIDATE! verdicts: -1 certified, 0 refused, 1
\ unresolvable.

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/core/pointer-storage.f

package RAW-LOAD-SEAL

\ ---- the nominal family a forged value would have to manufacture -------------
NEWTYPE rlsk

\ ---- one cell per raw storage definer, all created by the native engine ------
variable VAR-CELL
create CRE-CELL 8 allot
7 constant CON-CELL
PTR-VARIABLE PTR-CELL
create BYTE-CELL 256 allot

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

\ ---- the same cells never hold an ADDRESS either ------------------------------
\ The same cells never launder an integer into an address with no TRUST row:
\ an `n` stored through one mention and fetched through the next is no cell
\ address for `@` nor byte address for `c@`. This is the forgeries' binding, a
\ raw cell refusing a term.
: PUN-VAR ( -- )
   s" P1 ( n -- n ) VAR-CELL ! VAR-CELL @ @" CHECK-QUIET-CANDIDATE! 0 T= ;

: PUN-VAR-STORE ( -- )
   s" P2 ( n n -- ) VAR-CELL ! VAR-CELL @ !" CHECK-QUIET-CANDIDATE! 0 T= ;

: PUN-CREATE ( -- )
   s" P3 ( n -- n ) CRE-CELL ! CRE-CELL @ @" CHECK-QUIET-CANDIDATE! 0 T= ;

: PUN-CONSTANT ( -- )
   s" P4 ( -- n ) CON-CELL @" CHECK-QUIET-CANDIDATE! 0 T= ;

: PUN-HERE ( -- )
   s" P5 ( n -- n ) here ! here @ @" CHECK-QUIET-CANDIDATE! 0 T= ;

: PUN-BYTE ( -- )
   s" P6 ( n -- u8 ) VAR-CELL ! VAR-CELL @ c@" CHECK-QUIET-CANDIDATE! 0 T= ;

\ The write half. Refusing only the fetch would leave the cell holding a real
\ address for the next mention to read back as whatever it liked.
: PUN-POINTER-IN ( -- )
   s" P7 ( ptr a -- ) VAR-CELL !" CHECK-QUIET-CANDIDATE! 0 T= ;

\ `0 ptr-field` answers a fully typed `ptr ptr b` unrelated to the cell, so
\ FORGE-VAR's program written through the field must be refused too.
: FORGE-FIELD ( -- )
   s" F6 ( n -- rlsk ) VAR-CELL ! VAR-CELL 0 ptr-field @ @" CHECK-QUIET-CANDIDATE! 0 T= ;

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
   s" S4 ( -- ptr ptr n ) PTR-CELL" CHECK-QUIET-CANDIDATE! -1 T= ;

: SCALAR-DOES ( -- )
   s" S5 ( -- n ) DOES-CELL" CHECK-QUIET-CANDIDATE! -1 T= ;

\ A create'd BYTE buffer is what raw storage is for, and it keeps certifying:
\ the cell's pointee binds to the `u8` con, which is a scalar. This is the
\ control that says the pointer rule fences addresses and not raw storage.
: SCALAR-BYTES ( -- )
   s" S6 ( -- ) BYTE-CELL 4 type" CHECK-QUIET-CANDIDATE! -1 T= ;

\ ---- the cells still work at runtime -----------------------------------------
: ROUND-TRIP ( -- )
   5 VAR-CELL !  VAR-CELL @ 5 T=
   6 CRE-CELL !  CRE-CELL @ 6 T=
   CON-CELL 7 T= ;

: FORGERIES ( -- )
   FORGE-VAR FORGE-CREATE FORGE-CONSTANT FORGE-PTR FORGE-DOES FORGE-FIELD ;

: PUNS ( -- )
   PUN-VAR PUN-VAR-STORE PUN-CREATE PUN-CONSTANT PUN-HERE PUN-BYTE
   PUN-POINTER-IN ;

: SCALARS ( -- )
   SCALAR-VAR SCALAR-CREATE SCALAR-CONSTANT SCALAR-PTR SCALAR-DOES
   SCALAR-BYTES ;

: RUN ( -- )
   T-RESET
   FORGERIES
   PUNS
   SCALARS
   ROUND-TRIP
   T-REPORT ;

RUN

;package

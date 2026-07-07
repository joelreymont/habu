\ drec-shape-test.f - checked-prim surface pins for the typed dictionary
\ record capability (habu-typed-dictionary-record-c67adddb).
\
\ The positive shapes below compiling at all is the load-time assertion:
\ they are the exact access forms the engine rewrite of the raw-offset
\ XREF-*/BFR-*/BP-SLOT-* boundaries relies on, each named after the
\ trusted word it replaces. The negative pins document the two PES gaps
\ the capability must close (src/core/checker.f, engine lane); flipping
\ either to accept must update this fixture deliberately in the same
\ commit as the PES change.
\ Run: bin/hb --load test/drec-shape-test.f

require lib/errors.f
require lib/test.f
require test/checker-assert.f

\ record-pointer arithmetic ( = TRUSTED: XREF-REC+ today)
: DRS-REC+ ( ptr a n -- ptr a ) + ;

\ numeric slot read ( = XREF-CELL@ over a trusted rec ptr today)
: DRS-CELL@ ( ptr a n -- n ) cells + @ ;

\ pointer slot read ( = XREF-PTR@ + TRUSTED: XREF-N>U8 today)
: DRS-PTR@ ( ptr a n -- ptr u8 ) ptr-field @ ;

\ inline-name byte view ( = TRUSTED: XREF-A>U8 today; STRUCT-BYTE+ is the
\ existing audited byte-view axiom in src/core/structures-effects.f)
: DRS-NAME-BYTES ( ptr a -- ptr u8 ) $18 STRUCT-BYTE+ ;

\ sealed friend-band reads ( = TRUSTED: SEAL-LATCH@/SEAL-NDICT@ today;
\ data-base is already PE-PTR-A in the PES table)
: DRS-LATCH@ ( -- n ) data-base FRIEND-LATCH-CELL + @ ;
: DRS-NDICT@ ( -- n ) data-base SEAL-NDICT-CELL + @ ;

: DRS-REJECTS ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

: DRS-LATCH-EQUIV ( -- )
   s" drs latch read matches boundary" T-LABEL
   DRS-LATCH@ SEAL-LATCH@ T= ;

: DRS-NDICT-EQUIV ( -- )
   s" drs ndict read matches boundary" T-LABEL
   DRS-NDICT@ SEAL-NDICT@ T= ;

: DRS-CELL-EQUIV ( -- )
   s" drs slot read matches boundary" T-LABEL
   data-base FRIEND-LATCH-CELL CELL / DRS-CELL@ SEAL-LATCH@ T= ;

: DRS-REC+-BASE ( -- )
   s" drs rec+ preserves base" T-LABEL
   data-base 0 DRS-REC+ data-base = TTRUE ;

: DRS-PTR-READS ( -- )
   s" drs pointer reads execute deterministically" T-LABEL
   data-base 0 DRS-PTR@ data-base 0 DRS-PTR@ = TTRUE
   s" drs name byte view executes deterministically" T-LABEL
   data-base DRS-NAME-BYTES data-base DRS-NAME-BYTES = TTRUE ;

\ gap 1: dbase@ has no pointer provenance (PES: -- n); the capability's
\ single audited mint (or a dbase@ PES change) closes it.
: DRS-GAP-DBASE ( -- )
   s" drs gap: dbase@ stays numeric" T-LABEL
   s" DRSX1 ( -- ptr a ) dbase@" DRS-REJECTS ;

\ gap 2: patch32 models ( n n -- ); a ptr overload row retires
\ XREF-PATCH32 by letting checked callers pass the typed slot pointer.
: DRS-GAP-PATCH32 ( -- )
   s" drs gap: patch32 takes no ptr" T-LABEL
   s" DRSX2 ( n ptr a -- ) patch32" DRS-REJECTS ;

: MAIN ( -- )
   T-RESET
   DRS-LATCH-EQUIV
   DRS-NDICT-EQUIV
   DRS-CELL-EQUIV
   DRS-REC+-BASE
   DRS-PTR-READS
   DRS-GAP-DBASE
   DRS-GAP-PATCH32
   T-REPORT ;

MAIN

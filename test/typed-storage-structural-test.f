\ typed-storage-structural-test.f - closed structural storage cells.
\ Run: bin/hb --load src/lib/errors.f src/lib/string.f src/lib/test.f src/lib/fs.f
\   src/habu/verify-source.f test/typed-storage-structural-test.f
\ TYPED-VARIABLE / TYPED-BUFFER admit a closed structural cell: a non-linear
\ one-cell con (n, bool, i64, the role tokens) or a `ptr` chain bottoming at
\ one.

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/habu/verify-source.f

package TYPED-STORAGE-STRUCTURAL-TEST

NEWTYPE tssk

\ a LINEAR con (CHECKER-DEFLINEAR registers class CT-LINEAR): noncopyable, so it
\ is not storable in either position however closed it is.
DEFLINEAR tsslin

\ Nominal, layout, typed-pointer and xt cells: test/typed-storage-test.f and
\ test/xt-cell-test.f. Raw-cell pointer readings:
\ test/compiler/raw-cell-pointer-refusals.f; the nominal mint:
\ test/typed-storage-test.f section 8.

\ A raw `variable` types its cell with an open var each site instantiates
\ afresh, so one raw cell certifies contradictory readings.
\ 1. Closed STRUCTURAL cells: three scalar shapes a raw cell would blur, each
\    pinned at its declaration, each refusing the other two.
TYPED-VARIABLE TSSN n
TYPED-VARIABLE TSSF bool
TYPED-VARIABLE TSSP ptr u8
2 TYPED-BUFFER TSSB n

: TSSN-PUT ( n -- ) TSSN ! ;
: TSSN-GET ( -- n ) TSSN @ ;
: TSSF-PUT ( bool -- ) TSSF ! ;
: TSSF-GET ( -- bool ) TSSF @ ;
: TSSB-PUT ( n n -- ) {: v:n i:n :} v i TSSB ! ;
: TSSB-GET ( n -- n ) TSSB @ ;

: SECTION-STRUCTURAL ( -- )
   \ each cell's own type round-trips through the checker ...
   s" W1 ( n -- ) TSSN !"              CHECK-QUIET-CANDIDATE! -1 T=
   s" W2 ( -- n ) TSSN @"              CHECK-QUIET-CANDIDATE! -1 T=
   s" W3 ( bool -- ) TSSF !"           CHECK-QUIET-CANDIDATE! -1 T=
   s" W4 ( -- bool ) TSSF @"           CHECK-QUIET-CANDIDATE! -1 T=
   s" W5 ( ptr u8 -- ) TSSP !"         CHECK-QUIET-CANDIDATE! -1 T=
   s" W6 ( -- ptr u8 ) TSSP @"         CHECK-QUIET-CANDIDATE! -1 T=
   \ ... and every OTHER reading of the same cell is refused
   s" W7 ( bool -- ) TSSN !"           CHECK-QUIET-CANDIDATE! 0 T=
   s" W8 ( -- ptr u8 ) TSSN @"         CHECK-QUIET-CANDIDATE! 0 T=
   s" W9 ( -- tssk ) TSSN @"           CHECK-QUIET-CANDIDATE! 0 T=
   s" WA ( n -- ) TSSF !"              CHECK-QUIET-CANDIDATE! 0 T=
   s" WB ( n -- ) TSSP !"              CHECK-QUIET-CANDIDATE! 0 T=
   s" WC ( -- n ) TSSP @"              CHECK-QUIET-CANDIDATE! 0 T=
   \ the accessor value itself: a span cell is addressed as ptr ptr u8
   s" WD ( -- ptr ptr u8 ) TSSP"       CHECK-QUIET-CANDIDATE! -1 T=
   s" WE ( -- ptr n ) TSSN"            CHECK-QUIET-CANDIDATE! -1 T=
   \ indexed structural buffer, both directions
   s" WF ( n n -- ) TSSB !"            CHECK-QUIET-CANDIDATE! -1 T=
   s" WG ( n -- n ) TSSB @"            CHECK-QUIET-CANDIDATE! -1 T=
   s" WH ( bool n -- ) TSSB !"         CHECK-QUIET-CANDIDATE! 0 T=
   \ live round-trips: zero image, then the stored value
   TSSN-GET 0 T=
   7 TSSN-PUT  TSSN-GET 7 T=
   0 0= TSSF-PUT  TSSF-GET TTRUE
   5 0 TSSB-PUT  6 1 TSSB-PUT
   0 TSSB-GET 5 T=
   1 TSSB-GET 6 T= ;

\ 2. Define-time admissibility: what a structural declaration accepts, and the
\    fail-closed edge. An open var cannot be pinned at all, so it still rejects.
TYPED-VARIABLE TSS-EVAL-A ptr u8
variable TSS-EVAL-U
: TSS-EVAL-RUN ( -- ) TSS-EVAL-A @ TSS-EVAL-U @ INCLUDE-EVALUATE ;
: TSS-EVAL ( ptr u8 n -- n ) TSS-EVAL-U ! TSS-EVAL-A ! [: TSS-EVAL-RUN ;] catch ;

: SECTION-ADMISSIBILITY ( -- )
   \ closed structural cons and closed pointer chains are admitted
   s" TYPED-VARIABLE TSS-OK-N n" TSS-EVAL 0 T=
   s" TYPED-VARIABLE TSS-OK-B bool" TSS-EVAL 0 T=
   s" TYPED-VARIABLE TSS-OK-I i64" TSS-EVAL 0 T=
   s" TYPED-VARIABLE TSS-OK-IDX idx" TSS-EVAL 0 T=
   s" TYPED-VARIABLE TSS-OK-FD fd" TSS-EVAL 0 T=
   s" TYPED-VARIABLE TSS-OK-PN ptr n" TSS-EVAL 0 T=
   s" TYPED-VARIABLE TSS-OK-PU ptr u8" TSS-EVAL 0 T=
   s" TYPED-VARIABLE TSS-OK-PPU ptr ptr u8" TSS-EVAL 0 T=
   \ an OPEN var has nothing to pin, in either position: still refused
   s" TYPED-VARIABLE TSS-BAD-A a" TSS-EVAL CHECKER-REJECT-RC T=
   s" TYPED-VARIABLE TSS-BAD-PA ptr a" TSS-EVAL CHECKER-REJECT-RC T=
   \ a bare atom carries no effect to pin either
   s" TYPED-VARIABLE TSS-BAD-XT xt" TSS-EVAL CHECKER-REJECT-RC T=
   \ a LINEAR con is closed and one cell, and still refused in BOTH positions:
   \ noncopyable
   s" TYPED-VARIABLE TSS-BAD-LIN tsslin" TSS-EVAL CHECKER-REJECT-RC T=
   s" TYPED-VARIABLE TSS-BAD-PLIN ptr tsslin" TSS-EVAL CHECKER-REJECT-RC T=
   \ the refused names never reached the dictionary
   s" TSS-BAD-A" 0 search-wl 0= TTRUE
   s" TSS-BAD-PA" 0 search-wl 0= TTRUE
   s" TSS-BAD-XT" 0 search-wl 0= TTRUE
   s" TSS-BAD-LIN" 0 search-wl 0= TTRUE
   s" TSS-BAD-PLIN" 0 search-wl 0= TTRUE ;

\ 3. Why a sub-cell integer stays out of the DIRECT stored-type position, and
\    why it is still fine inside a pointer chain.
PTR-VARIABLE TSS-PV

: SECTION-SUBCELL-REASON ( -- )
   \ a stored u8/u16/char is refused ...
   s" TYPED-VARIABLE TSS-BAD-U8 u8" TSS-EVAL CHECKER-REJECT-RC T=
   s" TYPED-VARIABLE TSS-BAD-U16 u16" TSS-EVAL CHECKER-REJECT-RC T=
   s" TSS-BAD-U8" 0 search-wl 0= TTRUE
   \ ... because cell @ over a concrete `ptr u8` is a checker error, so the cell
   \ would be unreadable by the operator its own ( -- ptr u8 ) implies ...
   s" U1 ( ptr u8 -- n ) @"                   CHECK-QUIET-CANDIDATE! 0 T=
   \ ... while the byte operator is exactly right for it ...
   s" U2 ( ptr u8 -- u8 ) c@"                 CHECK-QUIET-CANDIDATE! -1 T=
   \ ... and one level up the span cell reads back fine, which is the shape
   \ TYPED-VARIABLE TSSP actually mints
   s" U3 ( ptr ptr u8 -- ptr u8 ) @"          CHECK-QUIET-CANDIDATE! -1 T=
   \ a raw pointer cell reached by ptr-field still enforces pointer-ness but NOT
   \ which pointer: the open pointee takes any of them, the same vacuity a raw
   \ scalar cell has, one level down
   s" U4 ( ptr a -- ) TSS-PV 0 ptr-field !"   CHECK-QUIET-CANDIDATE! -1 T=
   s" U5 ( ptr u8 -- ) TSS-PV 0 ptr-field !"  CHECK-QUIET-CANDIDATE! -1 T=
   s" U6 ( n -- ) TSS-PV 0 ptr-field !"       CHECK-QUIET-CANDIDATE! 0 T= ;

: RUN ( -- )
   T-RESET
   SECTION-STRUCTURAL
   SECTION-ADMISSIBILITY
   SECTION-SUBCELL-REASON
   T-REPORT ;

RUN

;package

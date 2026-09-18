\ typed-storage-structural-test.f - closed STRUCTURAL storage cells
\ (dot habu-typed-storage-sweep-b2cd1a61).
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f
\   src/habu/verify-source.f test/checker-assert.f
\   test/typed-storage-structural-test.f
\
\ Pins the fourth admissible stored type of TYPED-VARIABLE / TYPED-BUFFER: a
\ CLOSED STRUCTURAL CELL — a non-linear one-cell con (n, bool, i64, the role
\ tokens) and a `ptr` chain bottoming at any non-linear con (`ptr u8`,
\ `ptr ptr u8`). The nominal, layout, typed-pointer and xt<effect> cells are
\ pinned by test/typed-storage-test.f and test/xt-cell-test.f; this file owns
\ only the structural half and the reason its one exclusion exists.
\
\ THE INVARIANT: a certification must not be vacuous. A raw `variable` types its
\ cell with an open var that every call site instantiates afresh, so ONE raw
\ cell certifies mutually contradictory readings — section 1 measures them, and
\ that is why an accessor over a raw cell had to be spelled `TRUSTED:` to stay
\ honest: its signature was an assertion the checker never checked. The
\ POINTER readings are no longer among them (dot habu-refuse-a-ptr-5ad2734e):
\ a cell that took an integer in and gave an address out was a `@` at any
\ address, so section 1 now measures where that line was drawn as well.
\ Pinning the cell type at its declaration is what makes the same signature
\ mean something, and section 2 measures the difference — the wrong type is
\ refused where the raw cell took it.
\
\ WHY THE ORIGINAL `BAD-N n` REJECT WAS RIGHT TO FLIP. That reject was a SCOPE
\ decision, never a soundness claim: the gate existed to mint NOMINAL cells, and
\ a plain scalar was held to be `variable`'s job. Section 1 is the measurement
\ that it is not. checker.f's own RAW discipline draws the line in the same
\ place — a raw cell "admits only a plain scalar representation and must NEVER
\ absorb a nominal atom, arity-0 family, layout, or nominal-bearing pointer" —
\ so admitting a CONCRETE con to the storage gate cannot weaken the nominal
\ fence, and section 2's nominal rejects hold it from this side too.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f
require src/habu/verify-source.f
require test/checker-assert.f

package TYPED-STORAGE-STRUCTURAL-TEST

7121 constant E-STORAGE

NEWTYPE tssk 0

\ a LINEAR con (CHECKER-DEFLINEAR registers class CT-LINEAR): noncopyable, so it
\ is not storable in either position however closed it is.
DEFLINEAR tsslin

\ =============================================================================
\ 1. NEGATIVE CONTROL: what a raw cell certifies, and why that is the problem.
\    This is not a bug being reported — it is the baseline the pinned cells in
\    section 2 improve on. If a future change makes one of the SCALAR readings
\    reject, the structural-cell capability has become redundant and should be
\    re-derived, so this section failing is informative either way. The pointer
\    readings below are the opposite: they reject, and the two rows that still
\    certify are open holes with a dot each, not a baseline to preserve.
\ =============================================================================
variable TSS-RAWCELL
PTR-VARIABLE TSS-DECLCELL

\ The row `PTR-FIELD:` generates for a record's pointer field: one free pointee
\ in, a pointer-to-pointer out, related by nothing. It certifies, and V8 below
\ measures what that costs while dot habu-refuse-ptr-field-331a9731 is open.
: TSS-RAWFIELD ( ptr a -- ptr ptr u8 ) 0 ptr-field ;

: SECTION-RAW-IS-VACUOUS ( -- )
   s" V1 ( n -- ) TSS-RAWCELL !"             CHECK-QUIET-CANDIDATE! -1 T=
   s" V2 ( -- n ) TSS-RAWCELL @"             CHECK-QUIET-CANDIDATE! -1 T=
   s" V5 ( -- bool ) TSS-RAWCELL @"          CHECK-QUIET-CANDIDATE! -1 T=
   \ the ONE thing a raw cell still cannot do is mint a nominal: the fence this
   \ change had to leave standing (typed-storage-test.f section 8)
   s" V7 ( -- tssk ) TSS-RAWCELL @"          CHECK-QUIET-CANDIDATE! 0 T=
   \ V3, V4 and V6 read -1 until dot habu-refuse-a-ptr-5ad2734e landed. They were
   \ the vacuity's sharp end rather than another reading of it: a cell that
   \ certifies BOTH `( n -- )` and `( ptr u8 -- )` carries an integer in and
   \ hands an address out, which is `@` and `c@` at any address with no TRUST
   \ row. A raw cell now refuses a pointer in either direction, so the three
   \ reject and the scalar readings above are the whole of what stays open.
   s" V3 ( ptr u8 -- ) TSS-RAWCELL !"        CHECK-QUIET-CANDIDATE! 0 T=
   s" V4 ( -- ptr u8 ) TSS-RAWCELL @"        CHECK-QUIET-CANDIDATE! 0 T=
   s" V6 ( -- ptr ptr u8 ) TSS-RAWCELL @"    CHECK-QUIET-CANDIDATE! 0 T=
   \ ---- the two launders that OUTLIVE that rule ------------------------------
   \ Both certify, and both are the same pun V3/V4 stopped, reached around the
   \ binding instead of through it. They are pinned here as -1 so the day either
   \ dot closes, this suite says which one and not "something changed".
   \
   \ V8: the field door through a parameter. TSS-RAWFIELD's base pointee is a
   \ declared free var, not a RAW one, so the ptr-field token sees nothing to
   \ refuse; the RAW cell then binds to that free var at the call and the row's
   \ unrelated `ptr ptr u8` result reads the cell as an address anyway.
   \ Dot habu-refuse-ptr-field-331a9731.
   s" V8 ( n -- u8 ) TSS-RAWCELL ! TSS-RAWCELL TSS-RAWFIELD @ c@"
                                             CHECK-QUIET-CANDIDATE! -1 T=
   \ V9: the view door into a DECLARED cell. byte-view and cell-view are
   \ type-level renames, so the pair reinterprets the pointer cell the rule
   \ prescribes as a scalar cell and stores an integer into it, which the
   \ declared `@` then hands back as an address.
   \ Dot habu-refuse-a-scalar-030be3ad.
   s" V9 ( n -- ) TSS-DECLCELL BYTE-VIEW CELL-VIEW !"
                                             CHECK-QUIET-CANDIDATE! -1 T= ;

\ =============================================================================
\ 2. Closed STRUCTURAL cells: the same three shapes the raw cell blurred, each
\    pinned at its declaration, each refusing the other two.
\ =============================================================================
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
   \ ... and every OTHER reading of the same cell is refused, which is exactly
   \ what the raw cell in section 1 waved through
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

\ =============================================================================
\ 3. Define-time admissibility: what a structural declaration accepts, and the
\    fail-closed edge. An open var cannot be pinned at all, so it still rejects.
\ =============================================================================
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
   s" TYPED-VARIABLE TSS-BAD-A a" TSS-EVAL E-STORAGE T=
   s" TYPED-VARIABLE TSS-BAD-PA ptr a" TSS-EVAL E-STORAGE T=
   \ a bare atom carries no effect to pin either
   s" TYPED-VARIABLE TSS-BAD-XT xt" TSS-EVAL E-STORAGE T=
   \ a LINEAR con is closed and one cell, and is still refused in BOTH positions
   \ — noncopyable means a storage cell may not hold it, and a `ptr` to one may
   \ not be stored either
   s" TYPED-VARIABLE TSS-BAD-LIN tsslin" TSS-EVAL E-STORAGE T=
   s" TYPED-VARIABLE TSS-BAD-PLIN ptr tsslin" TSS-EVAL E-STORAGE T=
   \ the refused names never reached the dictionary
   s" TSS-BAD-A" 0 search-wl 0= TTRUE
   s" TSS-BAD-PA" 0 search-wl 0= TTRUE
   s" TSS-BAD-XT" 0 search-wl 0= TTRUE
   s" TSS-BAD-LIN" 0 search-wl 0= TTRUE
   s" TSS-BAD-PLIN" 0 search-wl 0= TTRUE ;

\ =============================================================================
\ 4. Why a sub-cell integer stays out of the DIRECT stored-type position, and
\    why it is still fine inside a pointer chain. Both halves are measured, so
\    the exclusion rests on a fact rather than on taste.
\ =============================================================================
PTR-VARIABLE TSS-PV

: SECTION-SUBCELL-REASON ( -- )
   \ a stored u8/u16/char is refused ...
   s" TYPED-VARIABLE TSS-BAD-U8 u8" TSS-EVAL E-STORAGE T=
   s" TYPED-VARIABLE TSS-BAD-U16 u16" TSS-EVAL E-STORAGE T=
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
   \ which pointer: the open pointee takes any of them, which is the same
   \ vacuity section 1 measures, one level down
   s" U4 ( ptr a -- ) TSS-PV 0 ptr-field !"   CHECK-QUIET-CANDIDATE! -1 T=
   s" U5 ( ptr u8 -- ) TSS-PV 0 ptr-field !"  CHECK-QUIET-CANDIDATE! -1 T=
   s" U6 ( n -- ) TSS-PV 0 ptr-field !"       CHECK-QUIET-CANDIDATE! 0 T= ;

: RUN ( -- )
   T-RESET
   SECTION-RAW-IS-VACUOUS
   SECTION-STRUCTURAL
   SECTION-ADMISSIBILITY
   SECTION-SUBCELL-REASON
   T-REPORT ;

RUN

;package

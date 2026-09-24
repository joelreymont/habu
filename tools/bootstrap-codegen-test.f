\ bootstrap-codegen-test.f - earliest-marker recovery cases absent from real builds.
\ Run: bin/hb --load tools/bootstrap-codegen-test.f

require lib/errors.f
require lib/string.f
require lib/test.f

include src/habu/hide.f

\ The watermark has to be read at top level BETWEEN the two duplicate fixture
\ records, and packages do not nest, so BCG-HIDE opens once to publish the
\ recording word and reopens below for the checks that read it.
package BCG-HIDE
private

variable MID                            \ ndict watermark between the duplicate fixture records

public

: MARK-MID ( -- )
   ndict@ MID ! ;

;package

\ Two packages export the same tail on purpose: the earlier record must win.
package BCG-DUP-EARLY
public
: DUP-MARK ( -- ) ;
;package

BCG-HIDE:MARK-MID

package BCG-DUP-LATE
public
: DUP-MARK ( -- ) ;
;package

package BCG-HIDE
private

: REC ( ptr u8 n -- n )
   BFR-FIND-FIRST-INDEX ;

: IMK-REC ( -- n )
   s" IMK-NDICT0" REC ;

: SEQ-REC ( -- n )
   s" SEQ" REC ;

\ The production markers exist in the live dictionary with IMK-NDICT0 (util.f's
\ first record) earlier than SEQ; the hide index must pick the earlier record
\ in either argument order.
: EARLIEST-MARKER ( -- )
   IMK-REC 0 >= TTRUE
   SEQ-REC 0 >= TTRUE
   IMK-REC SEQ-REC < TTRUE
   s" IMK-NDICT0" s" SEQ" BFR-MARKER-INDEX IMK-REC T=
   s" SEQ" s" IMK-NDICT0" BFR-MARKER-INDEX IMK-REC T= ;

\ Earliest-hide depends on FIND-FIRST returning the FIRST record of a name: the
\ duplicate fixture record published before the MID watermark must win, and the
\ match must fold case like the shell's BOOT-XREF-STR=CI. The dictionary record
\ of a package word stores its bare tail, so the searched name is `DUP-MARK`.
\ Naming both fixture words keeps the duplicate load-bearing: if either package
\ stopped publishing the tail, the file would fail to load here instead of
\ leaving the index assertions below trivially satisfiable by a single record.
: FIRST-RECORD ( -- )
   BCG-DUP-EARLY:DUP-MARK
   BCG-DUP-LATE:DUP-MARK
   s" DUP-MARK" REC 0 >= TTRUE
   s" DUP-MARK" REC MID @ < TTRUE
   s" dup-mark" REC s" DUP-MARK" REC T= ;

\ One marker missing falls back to the found one; both missing is asserted at
\ the component level (FIND -> NOT-FOUND, MIN-FOUND keeps NOT-FOUND) because
\ BFR-MARKER-INDEX's both-missing path is a process exit (die 76) by design.
: MISSING-FALLBACK ( -- )
   s" IMK-NDICT0" s" BCG-NO-SUCH-MARKER" BFR-MARKER-INDEX IMK-REC T=
   s" BCG-NO-SUCH-MARKER" s" IMK-NDICT0" BFR-MARKER-INDEX IMK-REC T=
   s" BCG-NO-SUCH-MARKER" REC BFR-NOT-FOUND T=
   BFR-NOT-FOUND BFR-NOT-FOUND BFR-MIN-FOUND BFR-NOT-FOUND T=
   5 BFR-REQUIRE-INDEX 5 T= ;

public

: TEST ( -- )
   EARLIEST-MARKER
   FIRST-RECORD
   MISSING-FALLBACK ;

;package

package BCG
public

: MAIN ( -- )
   T-RESET
   BCG-HIDE:TEST
   T-REPORT
   s" bootstrap-codegen-test: ok" type cr ;

;package

BCG:MAIN

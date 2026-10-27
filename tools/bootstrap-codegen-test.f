\ bootstrap-codegen-test.f - hide-marker recovery cases absent from real builds.
\ Run: bin/hb --load tools/bootstrap-codegen-test.f

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f

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
: DUP-MARK-WITH-A-LONG-SPELLING ( -- ) ;
;package

package BCG-HIDE
private

: REC ( ptr u8 n -- n )
   BOOT-XREF-FIND-INDEX ;

: IMK-REC ( -- n )
   s" IMK-NDICT0" REC ;

\ The production marker, IMK-NDICT0 (util.f's first record), exists in the live
\ dictionary and the hide index answers its record.
: MARKER ( -- )
   IMK-REC 0 >= TTRUE
   s" IMK-NDICT0" BOOT-XREF-MARKER-INDEX IMK-REC T= ;

\ Earliest-hide depends on lookup returning the FIRST record of a name: the
\ duplicate fixture record published before the MID watermark must win, and the
\ match must fold case. The dictionary record
\ of a package word stores its bare tail, so the searched name is `DUP-MARK`.
\ Naming both fixture words keeps the duplicate load-bearing: if either package
\ stopped publishing the tail, the file would fail to load here instead of
\ leaving the index assertions below trivially satisfiable by a single record.
: FIRST-RECORD ( -- )
   BCG-DUP-EARLY:DUP-MARK
   BCG-DUP-LATE:DUP-MARK
   BCG-DUP-LATE:DUP-MARK-WITH-A-LONG-SPELLING
   s" DUP-MARK" REC 0 >= TTRUE
   s" DUP-MARK" REC MID @ < TTRUE
   s" dup-mark" REC s" DUP-MARK" REC T=
   s" DUP-MARK-WITH-A-LONG-SPELLING" REC 0 >= TTRUE
   s" dup-mark-with-a-long-spelling" REC
   s" DUP-MARK-WITH-A-LONG-SPELLING" REC T= ;

\ A missing marker is asserted at the component level (FIND -> NOT-FOUND)
\ because BOOT-XREF-MARKER-INDEX's missing path is a process exit (die 76) by
\ design.
: MISSING ( -- )
   s" BCG-NO-SUCH-MARKER" REC -1 T= ;

public

: TEST ( -- )
   MARKER
   FIRST-RECORD
   MISSING ;

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

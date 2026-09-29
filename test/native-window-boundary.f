\ native-window-boundary.f - the window's checker certifies the dictionary
\ boundary and the capture seam at the optimizing tier.
\
\ Each case runs test/native-window-owner-child.f at tier 1 with the source
\ loader and layout the fixture's require closure needs, then the fixture: the
\ actual dictionary boundary compiled against a fresh owner, then the fresh
\ checker's final capture seam and later tape reuse. Each must reach
\ `window: 0` with nothing on stderr. The other source cases are
\ test/native-window-source.f and test/native-window-payload.f, gate rows of
\ their own: each compiles the window's whole core prefix at tier 1.
\
\ Registered as `TEST:SUITE native-window-boundary`. Run standalone:
\   bin/hb --load test/native-window-boundary.f

require lib/test.f
require test/native-window-owner-lib.f

package NW-OWNER-TEST

: BOUNDARY-CASES ( -- )
   s" native-window-boundary" PREPARE
   s" test/native-window-owner-fixed.f" WINDOW-SOURCE
   s" test/native-window-tape-detach.f" WINDOW-SOURCE ;

\ Public so the driver below runs it with the package closed.
public

: BOUNDARY-RUN ( -- )
   [: BOUNDARY-CASES ;] RUN-CASES
   s" native-window-boundary: ok" type cr ;

;package

NW-OWNER-TEST:BOUNDARY-RUN

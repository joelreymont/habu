\ native-window-payload.f - payload validation and preparation reach the
\ window's replacement checker at the optimizing tier.
\
\ The case runs test/native-window-owner-child.f at tier 1 with the source
\ loader and layout the fixture's require closure needs, then
\ test/native-window-owner-payload.f, and must reach `window: 0` with nothing
\ on stderr. The other source cases are test/native-window-source.f and
\ test/native-window-boundary.f, gate rows of their own: each compiles the
\ window's whole core prefix at tier 1.
\
\ Registered as `TEST:SUITE native-window-payload`. Run standalone:
\   bin/hb --load test/native-window-payload.f

require lib/test.f
require test/native-window-owner-lib.f

package NW-OWNER-TEST

: PAYLOAD-CASES ( -- )
   s" native-window-payload" PREPARE
   s" test/native-window-owner-payload.f" WINDOW-SOURCE ;

\ Public so the driver below runs it with the package closed.
public

: PAYLOAD-RUN ( -- )
   [: PAYLOAD-CASES ;] RUN-CASES
   s" native-window-payload: ok" type cr ;

;package

NW-OWNER-TEST:PAYLOAD-RUN

\ native-window-source.f - the window's checker certifies a source-compiled
\ compiler adapter and family readers at the optimizing tier.
\
\ Each case runs test/native-window-owner-child.f at tier 1 with the source
\ loader and layout the fixture's require closure needs, then the fixture: the
\ adapter, then the family readers. Each must reach `window: 0` with nothing on
\ stderr. The other source cases are test/native-window-boundary.f and
\ test/native-window-payload.f, and the tier-0 and handover cases
\ test/native-window-owner.f, gate rows of their own: each source case compiles
\ the window's whole core prefix at tier 1, about 22 s apiece.
\
\ Registered as `TEST:SUITE native-window-source`. Run standalone:
\   bin/hb --load test/native-window-source.f

require lib/test.f
require test/native-window-owner-lib.f

package NW-OWNER-TEST

: SOURCE-CASES ( -- )
   s" native-window-source" PREPARE
   s" test/native-window-owner-adapter.f" WINDOW-SOURCE
   s" test/native-window-owner-family.f" WINDOW-SOURCE ;

\ Public so the driver below runs it with the package closed.
public

: SOURCE-RUN ( -- )
   [: SOURCE-CASES ;] RUN-CASES
   s" native-window-source: ok" type cr ;

;package

NW-OWNER-TEST:SOURCE-RUN

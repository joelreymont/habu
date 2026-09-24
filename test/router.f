require lib/test.f
require test/referee-at.f

\ A test-only view of the router's transient storage. A released mapping
\ refuses every index, which is how the suite sees that a routing session
\ gave its search tables back.
package ROUTER
public

: SEARCH-STORAGE-FREE? ( -- bool )
   [: 0 GRID-ID drop ;] catch 7122 <> if false exit then
   [: 0 MAP-STATE drop ;] catch 7122 <> if false exit then
   [: 0 HEAP drop ;] catch 7122 <> if false exit then
   [: 0 NEW-ITEM drop ;] catch 7122 <> if false exit then
   [: 0 HISTORY-KEY drop ;] catch 7122 <> if false exit then
   true ;

;package

package ROUTER-TEST
using QTY
using PCB
using REFEREE-PIN

TYPED-VARIABLE SIGNAL net-id
TYPED-VARIABLE LEFT-PAD pad-id
TYPED-VARIABLE RIGHT-PAD pad-id
TYPED-VARIABLE FIRST-PAD pad-id
TYPED-VARIABLE FIRST-NET net-id
variable HASH

: XY ( n n -- point ) {: x:n y:n :} x QTY:MM y QTY:MM AT ;


: ENDPOINT-ON ( ptr u8 n net-id point layer-id -- pad-id )
   {: name:ptr u:n net:net-id at layer:layer-id :}
   name u PCB:CELL {: cell:cell-id :}
   cell s" J1" s" testpoint" 0 NM 0 NM LOCAL 0 DEG PART {: part:part-id :}
   part net s" 1" 0 NM 0 NM LOCAL 500 UM layer ROUND-PAD {: pad:pad-id :}
   pad PORT cell at 0 DEG PLACE pad ;


: ENDPOINT ( ptr u8 n net-id point -- pad-id ) TOP ENDPOINT-ON ;


: RESET ( n -- ) {: layers:n :}
   20 QTY:MM 12 QTY:MM 100 UM layers BOARD
   s" signal" NET SIGNAL !
   s" left" SIGNAL @ 2 6 XY ENDPOINT LEFT-PAD !
   s" right" SIGNAL @ 18 6 XY ENDPOINT RIGHT-PAD ! ;


: ROUTE-SIGNAL ( -- ROUTER:status )
   SIGNAL @ 500 UM 200 UM 600 UM 300 UM ROUTER:ROUTE ;


: LIVE-ITEMS ( -- n )
   0 ITEMS 0 ?do i ITEM-LIVE? if 1+ then loop ;


: VIA-COUNT ( -- n )
   0 ITEMS 0 ?do
      i ITEM-LIVE? if i ITEM@ ITEM-KIND PCB-KIND:TAG 3 = if 1+ then then
   loop ;


: HASH+ ( n -- ) HASH @ 31 * + 100000007 mod HASH ! ;


: ROUTE-HASH ( -- n )
   0 HASH !
   ITEMS 0 ?do
      i ITEM-LIVE? if
         i ITEM@ ITEM-ROUTE? if
            i ITEM@ ITEM-START POINT-X NM@ HASH+
            i ITEM@ ITEM-START POINT-Y NM@ HASH+
            i ITEM@ ITEM-END POINT-X NM@ HASH+
            i ITEM@ ITEM-END POINT-Y NM@ HASH+
            i ITEM@ ITEM-LAYER-MASK HASH+
            i ITEM@ ITEM-WIDTH NM@ HASH+
            i ITEM@ ITEM-DRILL NM@ HASH+
         then
      then
   loop HASH @ ;


: CONNECTED ( -- )
   GEOM:CHECK GEOM:CLEAR? TTRUE
   LEFT-PAD @ RIGHT-PAD @ GEOM:CONNECTED? TTRUE
   GEOM:OPEN-PORTS 0 T= ;


\ Two round endpoints whose copper meets at exactly one point are not connected
\ -- KiCad joins a shape a radius inflates only where it interpenetrates, and
\ the model's contact is the same word -- so the router has work to do on them
\ and does it, where it answered `already` before.  A nanometre of overlap
\ leaves nothing to route.  Each of the three boards is written and judged by
\ kicad-cli 10.0.5 below rather than claimed here.
: TANGENT-RESET ( n -- ) {: gap:n :}
   20 QTY:MM 12 QTY:MM 100 UM 2 BOARD
   s" signal" NET SIGNAL !
   s" left" SIGNAL @ 2 6 XY ENDPOINT LEFT-PAD !
   s" right" SIGNAL @ gap NM 6 QTY:MM AT ENDPOINT RIGHT-PAD ! ;


: TANGENT ( -- )
   2500000 TANGENT-RESET
   GEOM:CHECK GEOM:CLEAR? TTRUE
   LEFT-PAD @ RIGHT-PAD @ GEOM:CONNECTED? TFALSE
   GEOM:OPEN-PORTS 2 T=
   GEOM:UNCONNECTED 1 T=
   s" router-tangent.kicad_pcb" 0 1 REFEREE-AT
   ROUTE-SIGNAL ROUTER:SUCCESS? TTRUE
   CONNECTED
   GEOM:UNCONNECTED 0 T=
   s" router-tangent-routed.kicad_pcb" 0 0 REFEREE-AT
   2499999 TANGENT-RESET
   LEFT-PAD @ RIGHT-PAD @ GEOM:CONNECTED? TTRUE
   GEOM:OPEN-PORTS 0 T=
   GEOM:UNCONNECTED 0 T=
   s" router-tangent-over.kicad_pcb" 0 0 REFEREE-AT
   ROUTE-SIGNAL ROUTER-STATUS:TAG 1 T= ;


: COALESCED-RUN ( -- )
   2 RESET ROUTE-SIGNAL ROUTER:SUCCESS? TTRUE CONNECTED
   \ Two pads and one trace, independent of the lattice pitch.
   LIVE-ITEMS 3 T=
   2 ITEM@ ITEM-START POINT-X NM@ 2000000 T=
   2 ITEM@ ITEM-END POINT-X NM@ 18000000 T= ;


: DIAGONAL-COUNT ( -- n )
   0 ITEMS 0 ?do i ITEM-LIVE? if
      i ITEM@ {: item:item-id :}
      item ITEM-ROUTE? item ITEM-KIND PCB-KIND:TAG 2 = and if
         item ITEM-START POINT-X NM@ item ITEM-END POINT-X NM@ - abs {: dx:n :}
         item ITEM-START POINT-Y NM@ item ITEM-END POINT-Y NM@ - abs {: dy:n :}
         dx dy = dx 1000000 > and if 1+ then
      then
   then loop ;


: ROUTE-COUNT ( -- n )
   0 ITEMS 0 ?do i ITEM-LIVE? if i ITEM@ ITEM-ROUTE? if 1+ then then loop ;


: BEND-PREFERENCE ( -- )
   false ROUTER:DIAGONALS!
   20 QTY:MM 12 QTY:MM 100 UM 2 BOARD
   s" signal" NET SIGNAL !
   s" left" SIGNAL @ 2 2 XY ENDPOINT LEFT-PAD !
   s" right" SIGNAL @ 18 10 XY ENDPOINT RIGHT-PAD !
   ROUTE-SIGNAL ROUTER:SUCCESS? TTRUE CONNECTED
   \ A bend cost must choose a short L over an equal-length staircase.
   ROUTE-COUNT 6 <= TTRUE
   true ROUTER:DIAGONALS! ;


: DIAGONAL-ROUTE ( -- )
   20 QTY:MM 12 QTY:MM 100 UM 2 BOARD
   s" signal" NET SIGNAL !
   s" left" SIGNAL @ 2 2 XY ENDPOINT LEFT-PAD !
   s" right" SIGNAL @ 18 10 XY ENDPOINT RIGHT-PAD !
   ROUTE-SIGNAL ROUTER:SUCCESS? TTRUE CONNECTED
   DIAGONAL-COUNT 0 > TTRUE VIA-COUNT 0 T=
   ROUTE-HASH {: original:n :}
   CLEAR-ROUTES ROUTE-SIGNAL ROUTER:SUCCESS? TTRUE CONNECTED
   ROUTE-HASH original T= ;


: OBSTRUCTED ( -- )
   1 RESET
   10 6 XY 2 QTY:MM 4 QTY:MM TOP KEEP-OUT drop
   ROUTE-SIGNAL ROUTER-STATUS:TAG 0 T=
   CONNECTED VIA-COUNT 0 T=
   ROUTER:EXPANSIONS 0 > TTRUE
   ROUTE-HASH {: hash-before:n :}
   ROUTE-SIGNAL ROUTER-STATUS:TAG 1 T=
   ROUTE-HASH hash-before T= ;


: FORCED-VIAS ( -- )
   2 RESET
   10 6 XY 2 QTY:MM 12 QTY:MM TOP KEEP-OUT drop
   ROUTE-SIGNAL ROUTER-STATUS:TAG 0 T=
   CONNECTED VIA-COUNT 2 T= ;


: ALLOWED-LAYERS ( -- )
   4 RESET
   10 6 XY 2 QTY:MM 12 QTY:MM TOP KEEP-OUT drop
   LIVE-ITEMS {: before:n :}
   SIGNAL @ 500 UM 200 UM 600 UM 300 UM 1 LAYER-SET
      ROUTER:ROUTE-ON ROUTER-STATUS:TAG 2 T=
   LIVE-ITEMS before T=
   \ Reserve both inner layers. New through-vias cross them, but tracks do not.
   500 UM 200 UM 600 UM 300 UM 9 LAYER-SET ROUTER:ALL-ON 0 T=
   CONNECTED VIA-COUNT 2 T=
   ITEMS 0 ?do
      i ITEM-LIVE? if
         i ITEM@ ITEM-ROUTE? if
            i ITEM@ ITEM-KIND PCB-KIND:TAG 2 = if
               i ITEM@ ITEM-LAYER-MASK 9 and 0<> TTRUE
               i ITEM@ ITEM-WIDTH NM@ 200000 T=
            else i ITEM@ ITEM-SPAN-MASK 15 T= then
         then
      then
   loop ;


: DISALLOWED-ENDPOINT ( -- )
   2 RESET LIVE-ITEMS {: before:n :}
   SIGNAL @ 500 UM 200 UM 600 UM 300 UM 2 LAYER-SET
      ROUTER:ROUTE-ON ROUTER-STATUS:TAG 2 T=
   ROUTER:SEARCH-STATES 0 T= LIVE-ITEMS before T=
   SIGNAL @ 500 UM 200 UM 600 UM 300 UM 0 LAYER-SET
      ROUTER:ROUTE-ON ROUTER-STATUS:TAG 4 T=
   LIVE-ITEMS before T=
   \ The default invocation must not inherit a previous call's layer mask.
   ROUTE-SIGNAL ROUTER:SUCCESS? TTRUE CONNECTED ;


: AUTHORED-ESCAPE ( cell-id length -- ) {: cell:cell-id offset:length :}
   offset NM@ 0<> if
      cell SIGNAL @ 0 NM 0 NM LOCAL offset 0 NM LOCAL 200 UM TOP TRACE drop
   then
   cell SIGNAL @ offset 0 NM LOCAL 600 UM 300 UM VIA drop ;


: EXISTING-ESCAPES ( n -- ) {: offset:n :}
   2 RESET
   LEFT-PAD @ PAD-PART PART-CELL offset QTY:MM AUTHORED-ESCAPE
   RIGHT-PAD @ PAD-PART PART-CELL offset negate QTY:MM AUTHORED-ESCAPE
   SIGNAL @ 500 UM 200 UM 600 UM 300 UM 2 LAYER-SET
      ROUTER:ROUTE-ON ROUTER:SUCCESS? TTRUE
   CONNECTED VIA-COUNT 2 T=
   ITEMS 0 ?do
      i ITEM-LIVE? if
         i ITEM@ ITEM-ROUTE? if
            i ITEM@ ITEM-KIND PCB-KIND:TAG 2 T=
            i ITEM@ ITEM-LAYER-MASK 2 T=
         then
      then
   loop ;


: MECHANICAL-PAD ( -- )
   2 RESET
   s" mount" PCB:CELL {: cell:cell-id :}
   cell s" H1" s" mounting hole" 0 NM 0 NM LOCAL 0 DEG PART
      0 NM 0 NM LOCAL PCB-KIND:CIRCLE 2 QTY:MM 2 QTY:MM
      0 NM 2 QTY:MM 0 LAYER-SET NPTH-PAD drop
   cell 10 6 XY 0 DEG PLACE
   ROUTE-SIGNAL ROUTER:SUCCESS? TTRUE CONNECTED ;


: NO-PATH ( -- )
   1 RESET
   10 6 XY 2 QTY:MM 12 QTY:MM TOP KEEP-OUT drop
   LIVE-ITEMS {: before:n :}
   ROUTE-SIGNAL ROUTER-STATUS:TAG 2 T=
   LIVE-ITEMS before T=
   GEOM:CHECK GEOM:CLEAR? TTRUE
   LEFT-PAD @ RIGHT-PAD @ GEOM:CONNECTED? TFALSE
   GEOM:OPEN-PORTS 2 T= ;


: PARTIAL-VIA-TRANSITION ( -- )
   20 QTY:MM 12 QTY:MM 100 UM 4 BOARD
   s" signal" NET SIGNAL !
   s" left" SIGNAL @ 2 6 XY TOP ENDPOINT-ON LEFT-PAD !
   s" right" SIGNAL @ 18 6 XY BOTTOM ENDPOINT-ON RIGHT-PAD !
   LEFT-PAD @ PAD-PART PART-CELL SIGNAL @ 2 QTY:MM 0 NM LOCAL
      600 UM 300 UM TOP 1 LAYER 3 LAYER-SET VIA-SPAN {: partial:item-id :}
   \ The inner-layer wall admits a through-via only through the gap at x=11.
   \ Treating the existing partial via as through would terminate the search
   \ on a shorter but disconnected candidate and miss this legal route.
   5 6 XY 10 QTY:MM 12 QTY:MM 2 LAYER KEEP-OUT drop
   16 6 XY 8 QTY:MM 12 QTY:MM 2 LAYER KEEP-OUT drop
   ROUTE-SIGNAL ROUTER:SUCCESS? TTRUE
   CONNECTED VIA-COUNT 2 T=
   partial ITEM-SPAN-MASK 3 T= ;


: DETERMINISTIC-MOVE ( -- )
   1 RESET
   10 6 XY 2 QTY:MM 4 QTY:MM TOP KEEP-OUT drop
   ROUTE-SIGNAL ROUTER-STATUS:TAG 0 T= ROUTE-HASH {: original:n :}
   RIGHT-PAD @ PAD-PART PART-CELL 18 8 XY 0 DEG PLACE
   LEFT-PAD @ RIGHT-PAD @ GEOM:CONNECTED? TFALSE
   ROUTE-SIGNAL ROUTER-STATUS:TAG 0 T= CONNECTED
   ROUTE-HASH original T<>
   RIGHT-PAD @ PAD-PART PART-CELL 18 6 XY 0 DEG PLACE
   ROUTE-SIGNAL ROUTER-STATUS:TAG 0 T= CONNECTED
   ROUTE-HASH original T= ;


: TRANSACTION ( -- )
   20 QTY:MM 12 QTY:MM 100 UM 1 BOARD
   s" first" NET FIRST-NET !
   s" first-a" FIRST-NET @ 2 2 XY ENDPOINT FIRST-PAD !
   s" first-b" FIRST-NET @ 8 2 XY ENDPOINT drop
   s" signal" NET SIGNAL !
   s" left-a" SIGNAL @ 2 6 XY ENDPOINT LEFT-PAD !
   s" left-b" SIGNAL @ 8 6 XY ENDPOINT drop
   s" right" SIGNAL @ 18 6 XY ENDPOINT RIGHT-PAD !
   10 6 XY 2 QTY:MM 12 QTY:MM TOP KEEP-OUT drop
   FIRST-NET @ 500 UM 200 UM 600 UM 300 UM ROUTER:ROUTE ROUTER:SUCCESS? TTRUE
   LIVE-ITEMS {: before:n :} ROUTE-HASH {: first-hash:n :}
   ROUTE-SIGNAL ROUTER-STATUS:TAG 2 T=
   LIVE-ITEMS before T= ROUTE-HASH first-hash T=
   GEOM:CHECK GEOM:CLEAR? TTRUE ;


: SEARCH-BOUND ( -- )
   1 RESET LIVE-ITEMS {: before:n :}
   SIGNAL @ 1 NM 200 UM 600 UM 300 UM ROUTER:ROUTE ROUTER-STATUS:TAG 3 T=
   LIVE-ITEMS before T=
   SIGNAL @ 0 NM 200 UM 600 UM 300 UM ROUTER:ROUTE ROUTER-STATUS:TAG 4 T= ;


: LARGE-BOARD-LOCAL-ROUTE ( -- )
   \ This large multilayer board implies more than 36 million
   \ lattice locations at this pitch. A local route needs only reached states.
   200 QTY:MM 150 QTY:MM 100 UM 12 BOARD
   s" signal" NET SIGNAL !
   s" left" SIGNAL @ 2 6 XY ENDPOINT LEFT-PAD !
   s" right" SIGNAL @ 18 6 XY ENDPOINT RIGHT-PAD !
   SIGNAL @ 100 UM 200 UM 600 UM 300 UM ROUTER:ROUTE ROUTER:SUCCESS? TTRUE
   CONNECTED
   ROUTER:SEARCH-STATES 20000 < TTRUE
   ROUTE-HASH {: original:n :}
   RIGHT-PAD @ PAD-PART PART-CELL 18 7 XY 0 DEG PLACE
   RIGHT-PAD @ PAD-PART PART-CELL 18 6 XY 0 DEG PLACE
   SIGNAL @ 100 UM 200 UM 600 UM 300 UM ROUTER:ROUTE ROUTER:SUCCESS? TTRUE
   ROUTE-HASH original T= ;


: PENDING-REFUSAL ( -- )
   1 RESET
   s" pending" PCB:CELL {: pending:cell-id :}
   GEOM:CHECK GEOM-VERDICT:TAG 5 T=
   LIVE-ITEMS {: before:n :}
   ROUTE-SIGNAL ROUTER-STATUS:TAG 4 T=
   LIVE-ITEMS before T=
   pending 0 0 XY 0 DEG PLACE
   ROUTE-SIGNAL ROUTER:SUCCESS? TTRUE ;


: DECLARE-SIGNAL ( -- )
   SIGNAL @ 150 UM 150 UM 250 UM 9 LAYER-SET 650 UM 300 UM
   PCB-NET--RULES:MAKE NET-RULES! ;


: CONFIGURED-ROUTING ( -- )
   4 RESET
   SIGNAL @ 500 UM ROUTER:ROUTE-NET ROUTER-STATUS:TAG 4 T=
   ROUTER:LAST-ERROR E-RULES T=
   DECLARE-SIGNAL
   10 6 XY 2 QTY:MM 12 QTY:MM TOP KEEP-OUT drop
   LIVE-ITEMS {: before:n :}
   SIGNAL @ 500 UM 100 UM 650 UM 300 UM ROUTER:ROUTE ROUTER-STATUS:TAG 4 T=
   ROUTER:EXPANSIONS 0 T= LIVE-ITEMS before T=
   SIGNAL @ 500 UM 250 UM 650 UM 300 UM 6 LAYER-SET ROUTER:ROUTE-ON
   ROUTER-STATUS:TAG 4 T= ROUTER:EXPANSIONS 0 T= LIVE-ITEMS before T=
   500 UM ROUTER:ALL-NETS 0 T= CONNECTED VIA-COUNT 2 T=
   ITEMS 0 ?do
      i ITEM-LIVE? if i ITEM@ ITEM-ROUTE? if
         i ITEM@ ITEM-KIND PCB-KIND:TAG 2 = if
            i ITEM@ ITEM-WIDTH NM@ 250000 T=
            i ITEM@ ITEM-LAYER-MASK 9 and 0<> TTRUE
         else
            i ITEM@ ITEM-WIDTH NM@ 650000 T= i ITEM@ ITEM-DRILL NM@ 300000 T=
         then
      then then
   loop
   ROUTE-HASH {: original:n :}
   SIGNAL @ 500 UM ROUTER:ROUTE-NET ROUTER-STATUS:TAG 1 T=
   ROUTE-HASH original T=
   CLEAR-ROUTES
   \ Explicit legacy parameters remain useful overrides of routing choices,
   \ constrained by the net's minimum width and permitted layer intersection.
   SIGNAL @ 500 UM 200 UM 600 UM 300 UM ROUTER:ROUTE ROUTER:SUCCESS? TTRUE
   CONNECTED
   ITEMS 0 ?do
      i ITEM-LIVE? if i ITEM@ ITEM-ROUTE? if
         i ITEM@ ITEM-KIND PCB-KIND:TAG 2 = if
            i ITEM@ ITEM-WIDTH NM@ 200000 T=
            i ITEM@ ITEM-LAYER-MASK 9 and 0<> TTRUE
         then
      then then
   loop ;


\ The search tables belong to one routing session. A route that succeeds and
\ a route that throws both hand them back; the capture hook is the backstop,
\ not the owner.
: STALE-ROUTE ( -- )
   FIRST-NET @ 500 UM ROUTER:ROUTE-NET drop ;


: STORAGE-RELEASED ( -- )
   2 RESET SIGNAL @ FIRST-NET !
   ROUTE-SIGNAL ROUTER:SUCCESS? TTRUE
   ROUTER:SEARCH-STORAGE-FREE? TTRUE
   500 UM 200 UM 600 UM 300 UM ROUTER:ALL drop
   ROUTER:SEARCH-STORAGE-FREE? TTRUE
   2 RESET
   [: STALE-ROUTE ;] E-IDENTITY TTHROWSQ
   ROUTER:SEARCH-STORAGE-FREE? TTRUE ;


public


: RUN ( -- )
   T-RESET TANGENT COALESCED-RUN BEND-PREFERENCE DIAGONAL-ROUTE OBSTRUCTED FORCED-VIAS ALLOWED-LAYERS DISALLOWED-ENDPOINT
   0 EXISTING-ESCAPES 2 EXISTING-ESCAPES MECHANICAL-PAD
   NO-PATH PARTIAL-VIA-TRANSITION
   DETERMINISTIC-MOVE TRANSACTION SEARCH-BOUND
   LARGE-BOARD-LOCAL-ROUTE PENDING-REFUSAL CONFIGURED-ROUTING
   STORAGE-RELEASED T-REPORT ;


;using
;using
;using
;package
ROUTER-TEST:RUN

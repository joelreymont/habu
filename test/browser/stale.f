\ stale.f - STALE, a fixture module whose hello states a turn protocol revision
\ other than test/browser/turn-revision.f's, as a module whose events and
\ records are stale would; built by `tools/hb-build.f -- --target wasm`, whose
\ entry is MAIN. test/browser/echo-test.f runs it under
\ src/lib/browser/host-cli.mjs, which must end it at that hello.

require src/lib/browser/event.f
require test/browser/turn-revision.f

package STALE
private
using BROWSER-EVENT

CAST: BYTES>N ( ptr u8 -- n )

\ A record field, eight bytes low first, appended to OUT.
: FIELD, ( n -- )
   8 0 do  dup $FF and emit  8 rshift  loop drop ;

public

\ Start's hello, one revision past the fixtures'; the host ends the module
\ there, so no other event reaches it.
: TURN ( -- )
   0 EVENT@ 0 = if
      0 FIELD,  EVENT BYTES>N FIELD,  TURN-REVISION:PROTOCOL 1+ FIELD,  0 FIELD,
   then ;

;using
;package

: MAIN ( -- ) STALE:TURN ;

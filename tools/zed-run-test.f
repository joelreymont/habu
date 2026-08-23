\ zed-run-test.f - checked unit tests for the ZED remote harness.
\
\ Availability policy, command construction, and outcome classification need no
\ device. Remote-device operation remains an explicitly invoked tool concern.
\
\ Run: bin/hb --load lib/test.f tools/zed-run-lib.f tools/zed-run-test.f

require lib/test.f
require tools/zed-run-lib.f

package ZED

\ ---- outcome classification (no device: PROC-CMD state is the load default) --

: CLASSIFY-TESTS ( -- )
   [: 0 >RC RUN-OK ;] 0 TTHROWSQ                    \ success: no throw
   [: 1 >RC RUN-OK ;] E-ZED-RC TTHROWSQ             \ nonzero rc: fail-closed
   [: 255 >RC RUN-OK ;] E-ZED-RC TTHROWSQ           \ tools may exit 255 (ptxas)
   [: 0 >RC UNREACH-OK ;] 0 TTHROWSQ                \ ping success: no throw
   [: 255 >RC UNREACH-OK ;] E-ZED-UNREACH TTHROWSQ  \ ping nonzero: transport
   [: 1 >RC UNREACH-OK ;] E-ZED-UNREACH TTHROWSQ ;

\ ---- remote command construction --------------------------------------------

: CMD-TESTS ( -- )
   CMD-RESET
   s" cd" CMD-TOK  s" /scr" CMD-TOK  s" &&" CMD-TOK  s" bin/hb" CMD-TOK
   CMD$ s" cd /scr && bin/hb" T$=
   CMD-RESET
   s" true" CMD-TOK
   CMD$ s" true" T$= ;

\ ---- mandatory-device policy (both modes) -------------------------------------

: NEED-DEVICE-TESTS ( -- )
   AVAILABLE? if
      [: NEED-DEVICE ;] 0 TTHROWSQ                  \ available: no throw
   else
      [: NEED-DEVICE ;] E-ZED-DISABLED TTHROWSQ     \ disabled: fail-closed
   then ;

: AVAIL-TESTS ( -- )
   s" "    ENV-AVAILABLE? TFALSE
   s" 0"   ENV-AVAILABLE? TFALSE
   s" 1"   ENV-AVAILABLE? TTRUE
   s" yes" ENV-AVAILABLE? TTRUE ;

: RUN-ALL ( -- )
   CLASSIFY-TESTS
   CMD-TESTS
   AVAIL-TESTS
   NEED-DEVICE-TESTS ;

T-RESET
RUN-ALL
T-REPORT

;package

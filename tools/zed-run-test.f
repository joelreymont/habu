\ zed-run-test.f - checked unit tests for the ZED remote harness.
\
\ The pure tests (availability policy, command construction, outcome
\ classification) need no device and run in the Mac gate context. The two device
\ smokes run only when HABU_ZED is set: one proves a remote `true` returns rc 0,
\ the other proves a remote `false` is fail-closed (E-ZED-RC). When HABU_ZED is
\ unset/0 the smokes SKIP explicitly with a printed reason.
\
\ Run: bin/hb --load lib/test.f tools/zed-run-lib.f tools/zed-run-test.f
\ Device smokes: HABU_ZED=1 bin/hb --load lib/test.f tools/zed-run-lib.f tools/zed-run-test.f

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

\ ---- device smokes (HABU_ZED gated) -----------------------------------------

: SMOKE-TESTS ( -- )
   AVAILABLE? 0= if s" device smokes need HABU_ZED" SKIP exit then
   PING
   [: s" true" RUN RUN-OK ;] 0 TTHROWSQ             \ remote true -> rc 0
   [: s" false" RUN RUN-OK ;] E-ZED-RC TTHROWSQ     \ remote false -> fail-closed
   s" true" HAVE-TOOL? TTRUE                        \ tool present remotely
   s" zrt-no-such-tool" HAVE-TOOL? TFALSE           \ tool absent remotely
   [: s" true" NEED-TOOL ;] 0 TTHROWSQ
   [: s" zrt-no-such-tool" NEED-TOOL ;] E-ZED-TOOLCHAIN TTHROWSQ
   s" device: remote true rc0, remote false fail-closed, tool probe on ZED" type cr ;

: AVAIL-TESTS ( -- )
   s" "    ENV-AVAILABLE? TFALSE
   s" 0"   ENV-AVAILABLE? TFALSE
   s" 1"   ENV-AVAILABLE? TTRUE
   s" yes" ENV-AVAILABLE? TTRUE ;

: RUN-ALL ( -- )
   CLASSIFY-TESTS
   CMD-TESTS
   AVAIL-TESTS
   NEED-DEVICE-TESTS
   SMOKE-TESTS ;

T-RESET
RUN-ALL
T-REPORT

end-package

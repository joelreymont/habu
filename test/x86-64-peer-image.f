\ Cross-build the real x64 pass-chain fixture into an executable for the peer.
\ The host checks the image; the peer must run hb-x64-peer with status 0 and
\ hb-x64-peer-negative with status 21. Neither image is a Habu engine.
require test/compiler/x64-chain.f
require test/x86-64-peer-harness.f

\ Reuse the same HIR and pass-row driver as the host suite. This adds execution
\ of its result; no second copy of the compiler fixture or private driver exists.
package X64CHAIN-TEST
private
: PEER-BODY ( IR-CTX:ctx -- )
   HIR-MOD BUILD-DIFF
   2 1 CHAIN {: m:IR-BUILD:module :}
   CC m X64HARNESS:POSITION NBACK:EMIT
   X64EMIT:BYTES X64EMIT:SIZE X64HARNESS:APPEND-ROUTINE
   CC NBACK:RETIRE
   CC NBACK:RELEASE ;
public
: PEER-ROUTINE ( -- ) WBND [: PEER-BODY ;] IR-CTX:WITH-CONTEXT ;
;package

package X64PEER
private
: BUILD ( bool ptr u8 n -- ) {: negative:bool path:ptr pathu:n :}
   negative X64HARNESS:OPEN,
   20 7 13 X64HARNESS:CASE2,
   7 20 -13 X64HARNESS:CASE2,
   -20 -7 -13 X64HARNESS:CASE2,
   X64HARNESS:MIN-CELL 1 X64HARNESS:MAX-CELL X64HARNESS:CASE2,
   X64HARNESS:MAX-CELL -1 X64HARNESS:MIN-CELL X64HARNESS:CASE2,
   X64HARNESS:CLOSE,
   X64HARNESS:ENTRY,
   X64CHAIN-TEST:PEER-ROUTINE
   path pathu X64HARNESS:WRITE-ELF ;

public
: RUN ( -- )
   T-RESET
   X64HARNESS:INIT
   false s" hb-x64-peer" TMP-PATH BUILD
   true s" hb-x64-peer-negative" TMP-PATH BUILD
   X64HARNESS:DISPOSE
   T-REPORT ;
;package

X64PEER:RUN

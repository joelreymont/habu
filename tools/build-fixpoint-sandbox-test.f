\ build-fixpoint-sandbox-test.f - checked fixture for tools/build-fixpoint.f: the
\ refresh refusing loudly in the stale-seed sandbox (tools/build-fixpoint-test-lib.f)
\ when a stage payload crashes or fails certification. A gate row of its own: one
\ row running every build-fixpoint case took 293-338 s in the gate's pool; the
\ rest are tools/build-fixpoint-test.f.
\ Run: bin/hb --load tools/build-fixpoint-sandbox-test.f

require src/lib/string.f
require tools/build-fixpoint-test-lib.f

\ The shared fixture's words are private words of the tool's package, so this
\ row reopens it the way tools/build-fixpoint-test-lib.f does.
package BUILD-FIXPOINT

\ Stale-seed install regression: a refresh child that dies (a crash baked into the
\ fixture's src/arch/arm64/mnem.f payload aborts the bootstrap child with SIGABRT
\ rc 134, as a seed that cannot load the current prefix does) fails the install
\ loudly: BF-BUILD-RC exit, a named stderr diagnostic, the engine byte-unchanged
\ and no stamp. The crash rides a TRUSTED: boundary: a `0 set-check` body is still
\ checked by VERIFY:SOURCE-BUF and would be rejected before it ran.
: BFT-STALE-SABOTAGE ( -- )
   s" src/arch/arm64/mnem.f" BFT-READ {: u:n :}
   BFT-STALE-PAYLOAD s" TRUSTED: BFT-STALE-CRASH ( -- ) 1 0 ! ; BFT-STALE-CRASH" FS:WRITE-ALL
   BFT-STALE-PAYLOAD BFT-NL 1 FS:APPEND-FILE
   BFT-STALE-PAYLOAD BFT-READ-BUF u FS:APPEND-FILE ;

: BFT-TEST-STALE-INSTALL ( -- )
   BFT-STALE-PREPARE
   BFT-STALE-SABOTAGE
   BFT-STALE-ARGV
   BFT-STALE-SPAWN {: outu:n erru:n rcn:n :}
   rcn BF-BUILD-RC T=
   BFT-BIG-ERR erru s" build-fixpoint: failed" CONTAINS? TTRUE
   BFT-BIG-ERR erru s" E-BUILD-STATUS" CONTAINS? TTRUE
   BFT-BIG-ERR erru s" habu-crash regs" CONTAINS? TTRUE
   BFT-STALE-HB s" bin/hb" BF-FILE= TTRUE
   BFT-STALE-STAMP FS:FILE? TFALSE ;

\ Staged-fixpoint refusal regression: a type-broken CHECKED definition appended
\ to the sandbox copy of src/arch/arm64/mnem.f, which the certify scan reaches
\ early, makes the refresh refuse at the blocking pre-pass: BF-BUILD-RC exit,
\ the certify diagnostic naming the word on stdout, E-BUILD-CERTIFY on stderr,
\ the sandbox engine byte-unchanged and no stamp. Runs in the BFT-STALE
\ sandbox; the workspace bin/hb is untouched.
: BFT-STALE-PAYLOAD-RESTORE ( -- )
   s" src/arch/arm64/mnem.f" BFT-READ {: u:n :}
   BFT-STALE-PAYLOAD BFT-READ-BUF u FS:WRITE-ALL ;

: BFT-CERT-INJ-SABOTAGE ( -- )
   BFT-STALE-PAYLOAD-RESTORE
   BFT-STALE-PAYLOAD s" : BFT-CERT-INJ ( n -- n ) drop ;" FS:APPEND-FILE
   BFT-STALE-PAYLOAD BFT-NL 1 FS:APPEND-FILE ;

: BFT-TEST-CERT-INJECT-INSTALL ( -- )
   BFT-CERT-INJ-SABOTAGE
   BFT-STALE-ARGV
   BFT-STALE-SPAWN {: outu:n erru:n rcn:n :}
   rcn BF-BUILD-RC T=
   BFT-BIG-OUT outu s" certify: stage2-src rejected" CONTAINS? TTRUE
   BFT-BIG-OUT outu s" bft-cert-inj" CONTAINS? TTRUE
   BFT-BIG-ERR erru s" build-fixpoint: failed" CONTAINS? TTRUE
   BFT-BIG-ERR erru s" E-BUILD-CERTIFY" CONTAINS? TTRUE
   BFT-STALE-HB s" bin/hb" BF-FILE= TTRUE
   BFT-STALE-STAMP FS:FILE? TFALSE ;

\ Public so the driver below runs with the package CLOSED, as a gate row must.
public
: BFT-SANDBOX-RUN ( -- )
   T-RESET
   BFT-PREPARE
   s" stale seed install" [: BFT-TEST-STALE-INSTALL ;] BFT-STEP
   s" cert inject install" [: BFT-TEST-CERT-INJECT-INSTALL ;] BFT-STEP
   s" build-fixpoint-sandbox-test: ok" BFT-FINISH ;

;package

BUILD-FIXPOINT:BFT-SANDBOX-RUN

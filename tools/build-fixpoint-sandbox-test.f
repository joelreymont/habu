\ build-fixpoint-sandbox-test.f - checked fixture for tools/build-fixpoint.f:
\ the refresh refusing loudly in the stale-seed sandbox (tools/build-fixpoint-test-lib.f)
\ when a stage payload crashes or fails certification. The build, the watermark
\ refusals on its capture host and the stamp cases are tools/build-fixpoint-test.f;
\ this is a gate row of its own, because one row running every build-fixpoint
\ case took 293-338 s in the gate's pool.
\ Run: bin/hb --load tools/build-fixpoint-sandbox-test.f

require tools/build-fixpoint-test-lib.f

\ The shared fixture's words are private words of the tool's package, so this
\ row reopens it the way tools/build-fixpoint-test-lib.f does.
package BUILD-FIXPOINT

\ Stale-seed install regression: a refresh child that dies (here: a crash baked
\ into the fixture's src/arch/arm64/mnem.f emitter payload, so the
\ bootstrap child aborts with SIGABRT rc 134 exactly like a seed that cannot
\ load the current engine prefix) must fail the install loudly: deterministic
\ BF-BUILD-RC exit, a named stderr diagnostic, the engine binary byte-unchanged,
\ and no stamp. Before the BF-CLI boundary the E-BUILD-STATUS throw escaped to
\ BTHROW's no-handler exit: silent, exit code masked to the low 8 bits.
\ The crash rides a TRUSTED: boundary so BLOCKING certification cannot see it
\ (a `0 set-check` body is still checked by VERIFY:SOURCE-BUF and would be
\ rejected statically before ever running) -- the stale-seed scenario this
\ models is a semantic runtime failure, not a type error.
: BFT-STALE-SABOTAGE ( -- )
   s" src/arch/arm64/mnem.f" BFT-READ {: u:n :}
   BFT-STALE-PAYLOAD s" TRUSTED: BFT-STALE-CRASH ( -- ) 1 0 ! ; BFT-STALE-CRASH" WRITE-ALL
   BFT-STALE-PAYLOAD BFT-NL 1 APPEND-FILE
   BFT-STALE-PAYLOAD BFT-READ-BUF u APPEND-FILE ;

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
   BFT-STALE-STAMP FILE? TFALSE ;

\ Staged-fixpoint refusal regression (dot habu-staged-fixpoint-src-0b5fc6e6):
\ a deliberately type-broken CHECKED definition in a source file of the
\ assembled stage list (here appended to the sandbox copy of src/arch/arm64/mnem.f, so the certify scan reaches it early) must make the
\ refresh REFUSE at the blocking pre-pass: deterministic BF-BUILD-RC exit, the
\ certify diagnostic naming the injected word on stdout, the E-BUILD-CERTIFY
\ name on stderr, the sandbox engine byte-unchanged, and no stamp. Runs in the
\ BFT-STALE sandbox tree (private tmp + scratch install target - the real
\ workspace bin/hb is never touched) with a fresh sabotage replacing the
\ stale-seed one.
: BFT-STALE-PAYLOAD-RESTORE ( -- )
   s" src/arch/arm64/mnem.f" BFT-READ {: u:n :}
   BFT-STALE-PAYLOAD BFT-READ-BUF u WRITE-ALL ;

: BFT-CERT-INJ-SABOTAGE ( -- )
   BFT-STALE-PAYLOAD-RESTORE
   BFT-STALE-PAYLOAD s" : BFT-CERT-INJ ( n -- n ) drop ;" APPEND-FILE
   BFT-STALE-PAYLOAD BFT-NL 1 APPEND-FILE ;

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
   BFT-STALE-STAMP FILE? TFALSE ;

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

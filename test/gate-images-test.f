\ The real gate adapter schedules a compiler supplier beside an independent
\ row. A private source tree replaces only the compiler builder at the normal
\ child process boundary. The builder cannot finish until that row releases
\ it, so a synchronous setup ends at the builder's bounded wait, which this
\ test reports as a timeout rather than a failure. Keep each registry,
\ builder, consumer copy and transcript for replay.
\ Run: bin/hb --load test/gate-images-test.f
require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-cwd.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require src/lib/test/subject.f
require test/suite-budget.f

package GATE-IMAGES-ORCH-E2E
private
$40000 constant IO-CAP
create ROOT FS-PATH-CAP allot variable ROOT-U
create PATH FS-PATH-CAP allot
create SOURCE FS-PATH-CAP allot
create OUT IO-CAP allot variable OUT-U
create ERR IO-CAP allot variable ERR-U
variable RC
variable PARENT-U

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;

: AT ( ptr u8 n -- ptr u8 n )
   ROOT$ 2swap PATH JOIN-PATH PATH swap ;

: COPY ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n dst:ptr dstu:n :}
   SOURCE-ROOT:CWD$ src srcu SOURCE JOIN-PATH {: u:n :}
   SOURCE u dst dstu AT COPY-FILE-STREAM ;

: LINK-DIR ( ptr u8 n -- )
   {: rel:ptr relu:n :}
   SOURCE-ROOT:CWD$ rel relu SOURCE JOIN-PATH {: u:n :}
   SOURCE u rel relu AT MAKE-SYMLINK ;

: TEST-FILE ( ptr u8 n -- )
   {: src:ptr srcu:n :}
   SOURCE-ROOT:CWD$ nip 1+ {: prefix:n :}
   src prefix + srcu prefix - AT {: dst:ptr dstu:n :}
   dst dstu EXISTS? if exit then
   0 PARENT-U !
   dstu 0 ?do dst i + c@ 47 = if i PARENT-U ! then loop
   dst PARENT-U @ MAKE-DIRS
   src srcu dst dstu MAKE-SYMLINK ;

: SETUP ( ptr u8 n -- )
   HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   s" bin" AT MAKE-DIRS
   s" test" AT MAKE-DIRS
   ENGINE-CANDIDATE:PATH$ s" bin/hb" AT MAKE-SYMLINK
   s" test/gate-images-orch-builder.f" s" test/compiler-images-build.f" COPY
   s" test/gate-images-orch-registry.f" s" test/gate-images-orch-registry.f" COPY
   s" test/gate-images-orch-release.f" s" test/gate-images-orch-release.f" COPY
   s" test/gate-images-orch-consumer.f" s" test/gate-images-orch-consumer.f" COPY
   s" test/gate-images-orch-later.f" s" test/gate-images-orch-later.f" COPY
   s" src" LINK-DIR s" tools" LINK-DIR
   SOURCE-ROOT:CWD$ s" test" SOURCE JOIN-PATH
      SOURCE swap [: TEST-FILE ;] WALK-FILES ;

: RESULT ( result<pcap:captured,pcap:failed> -- )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! 0 RC ! ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len code:rc :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! code RC>N RC ! ENDOF
   ;MATCH ;

: LOG ( ptr u8 n ptr u8 n -- )
   {: outname:ptr outu:n errname:ptr erru:n :}
   outname outu AT OUT OUT-U @ WRITE-ALL
   errname erru AT ERR ERR-U @ WRITE-ALL ;

: RUN-GATE ( bool -- ) {: fail:bool :}
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/gate-images-orch-registry.f" >LEN PROC-ARGV+
   s" HABU_GATE_ORCH_EVIDENCE" >LEN ROOT$ >LEN PROC-ENV+
   s" HABU_GATE_ORCH_FAIL" >LEN
      fail if s" 1" else s" 0" then >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN ROOT$ >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN SUITE-BUDGET:CHILD-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE RESULT ;

\ A nested gate whose compiler bundle ran out of time exits PROC-TIMEOUT-RC
\ (gate-images-orch-registry.f). Its transcript is already saved; the deadline
\ is thrown again, never asserted as an exit code.
: TIMED-OUT ( -- )
   s" test/gate-images-orch-registry.f" OUT OUT-U @ ERR ERR-U @ RC @
   SUBJECT:RC-TIMED-OUT ;

: HAS-OUT ( ptr u8 n -- bool )
   OUT OUT-U @ 2swap CONTAINS? ;

: CHECK-SUCCESS ( -- )
   RC @ 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   RC @ 0 T=
   s" pair/hb-root" AT EXECUTABLE? TTRUE
   s" pair/hb-ordinary" AT FILE? TTRUE
   s" pair/hb-root.map" AT FILE? TTRUE
   s" consumer-started" AT FILE? TTRUE
   s" consumer" AT FILE? TTRUE
   s" later" AT FILE? TTRUE
   s" builder-released" AT FILE? TTRUE
   s" PASS: release" HAS-OUT TTRUE
   s" PASS: compiler-images-build" HAS-OUT TTRUE
   s" PASS: consumer" HAS-OUT TTRUE ;

: CHECK-FAILURE ( -- )
   RC @ 0<> TTRUE
   s" consumer-started" AT FILE? TFALSE
   s" consumer" AT FILE? TFALSE
   s" later" AT FILE? TTRUE
   s" RED: compiler-images-build kind=exit code=73" HAS-OUT TTRUE
   s" RED: consumer kind=exit code=75" HAS-OUT TTRUE
   s" PASS: later" HAS-OUT TTRUE
   OUT-U @ 32768 > TTRUE
   ERR-U @ 32768 > TTRUE
   s" compiler-images stdout (full):" HAS-OUT TTRUE
   s" builder stdout prefix marker" HAS-OUT TTRUE
   s" builder stdout suffix marker" HAS-OUT TTRUE
   ERR ERR-U @ s" compiler-images stderr (full):" CONTAINS? TTRUE
   ERR ERR-U @ s" builder stderr prefix marker" CONTAINS? TTRUE
   ERR ERR-U @ s" builder stderr suffix marker" CONTAINS? TTRUE ;

public
: RUN ( -- )
   T-RESET
   s" gate-images-orch-success" SETUP
   false RUN-GATE
   s" success.out" s" success.err" LOG
   TIMED-OUT
   s" the independent row releases the bundle before dependent consumers" T-LABEL
   CHECK-SUCCESS
   s" success artifacts: " type ROOT$ type cr
   s" gate-images-orch-failure" SETUP
   true RUN-GATE
   s" failure.out" s" failure.err" LOG
   TIMED-OUT
   s" a failed bundle leaves dependents red and later independent work green" T-LABEL
   CHECK-FAILURE
   s" failure artifacts: " type ROOT$ type cr
   T-REPORT ;
;package

GATE-IMAGES-ORCH-E2E:RUN

\ The real gate adapter schedules a compiler supplier beside independent rows; a
\ private source tree replaces only the compiler builder at the child process
\ boundary. The builder cannot finish until the release row before the consumer
\ and the later row after it have run, so a synchronous setup, or a consumer
\ holding the registry while it waits for the bundle, ends at the builder's
\ bounded wait, and the bundle's build row and this test go red.
require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/fs-mutate.f
require src/lib/process-cwd.f
require src/lib/process-env.f
require src/lib/engine-candidate.f
require src/lib/process.f
require test/pcap-result.f

package GATE-IMAGES-ORCH-E2E
private
$40000 constant IO-CAP
create ROOT PATH-CAP allot variable ROOT-U
create PATH PATH-CAP allot
create SOURCE PATH-CAP allot
create OUT IO-CAP allot variable OUT-U
create ERR IO-CAP allot variable ERR-U
variable RC
variable PARENT-U

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;

: AT ( ptr u8 n -- ptr u8 n )
   ROOT$ 2swap PATH FS:JOIN-PATH PATH swap ;

: COPY ( ptr u8 n ptr u8 n -- )
   {: src:ptr srcu:n dst:ptr dstu:n :}
   SOURCE-ROOT:CWD$ src srcu SOURCE FS:JOIN-PATH {: u:n :}
   SOURCE u dst dstu AT FS:COPY-FILE-STREAM ;

: LINK-DIR ( ptr u8 n -- )
   {: rel:ptr relu:n :}
   SOURCE-ROOT:CWD$ rel relu SOURCE FS:JOIN-PATH {: u:n :}
   SOURCE u rel relu AT FS:MAKE-SYMLINK ;

: TEST-FILE ( ptr u8 n -- )
   {: src:ptr srcu:n :}
   SOURCE-ROOT:CWD$ nip 1+ {: prefix:n :}
   src prefix + srcu prefix - AT {: dst:ptr dstu:n :}
   dst dstu FS:EXISTS? if exit then
   0 PARENT-U !
   dstu 0 ?do dst i + c@ 47 = if i PARENT-U ! then loop
   dst PARENT-U @ FS:MAKE-DIRS
   src srcu dst dstu FS:MAKE-SYMLINK ;

: SETUP ( ptr u8 n -- )
   FS:MKTEMP-DIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   s" bin" AT FS:MAKE-DIRS
   s" test" AT FS:MAKE-DIRS
   ENGINE-CANDIDATE:PATH$ s" bin/hb" AT FS:MAKE-SYMLINK
   s" test/gate-images-orch-builder.f" s" test/compiler-images-build.f" COPY
   s" test/gate-images-orch-registry.f" s" test/gate-images-orch-registry.f" COPY
   s" test/gate-images-orch-release.f" s" test/gate-images-orch-release.f" COPY
   s" test/gate-images-orch-consumer.f" s" test/gate-images-orch-consumer.f" COPY
   s" test/gate-images-orch-later.f" s" test/gate-images-orch-later.f" COPY
   s" src" LINK-DIR s" tools" LINK-DIR
   SOURCE-ROOT:CWD$ s" test" SOURCE FS:JOIN-PATH
      SOURCE swap [: TEST-FILE ;] FS:WALK-FILES ;

: RESULT ( result<pcap:captured,pcap:failed> -- )
   PCAP-RESULT:>N RC ! ERR-U ! OUT-U ! ;

\ Registries and transcripts are kept.
: LOG ( ptr u8 n ptr u8 n -- )
   {: outname:ptr outu:n errname:ptr erru:n :}
   outname outu AT OUT OUT-U @ FS:WRITE-ALL
   errname erru AT ERR ERR-U @ FS:WRITE-ALL ;

: RUN-GATE ( bool -- ) {: fail:bool :}
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/gate-images-orch-registry.f" >LEN PROC-ARGV+
   s" HABU_GATE_ORCH_EVIDENCE" >LEN ROOT$ >LEN PROC-ENV+
   s" HABU_GATE_ORCH_FAIL" >LEN
      fail if s" 1" else s" 0" then >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   ENGINE-CANDIDATE:PATH$ >LEN ROOT$ >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN PROCESS:WAIT-FOREVER
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE RESULT ;

: HAS-OUT ( ptr u8 n -- bool )
   OUT OUT-U @ 2swap CONTAINS? ;

: CHECK-SUCCESS ( -- )
   RC @ 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   RC @ 0 T=
   s" pair/hb-root" AT FS:EXECUTABLE? TTRUE
   s" pair/hb-ordinary" AT FS:FILE? TTRUE
   s" pair/hb-root.map" AT FS:FILE? TTRUE
   s" pair/hb-ordinary.map" AT FS:FILE? TTRUE
   s" consumer-started" AT FS:FILE? TTRUE
   s" consumer" AT FS:FILE? TTRUE
   s" later" AT FS:FILE? TTRUE
   s" builder-released" AT FS:FILE? TTRUE
   s" PASS: release" HAS-OUT TTRUE
   s" PASS: compiler-images-build" HAS-OUT TTRUE
   s" PASS: consumer" HAS-OUT TTRUE ;

: CHECK-FAILURE ( -- )
   RC @ 0<> TTRUE
   s" consumer-started" AT FS:FILE? TFALSE
   s" consumer" AT FS:FILE? TFALSE
   s" later" AT FS:FILE? TTRUE
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
   s" the release and later rows run beside the bundle a consumer waits for" T-LABEL
   CHECK-SUCCESS
   s" success artifacts: " type ROOT$ type cr
   s" gate-images-orch-failure" SETUP
   true RUN-GATE
   s" failure.out" s" failure.err" LOG
   s" a failed bundle leaves dependents red and later independent work green" T-LABEL
   CHECK-FAILURE
   s" failure artifacts: " type ROOT$ type cr
   T-REPORT ;
;package

GATE-IMAGES-ORCH-E2E:RUN

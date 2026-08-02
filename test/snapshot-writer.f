\ snapshot-writer.f - adversarial proofs for the snapshot image writer.
\
\ Two properties of src/habu/snap-lib.f, each driven through a real snapshot
\ build in a child process (SNAPGO forgets its definitions and exits, so the
\ writer path can only be exercised out of process):
\
\   1. Return-stack zeroing. test/snapshot-writer-poison.f plants fixed
\      non-zero canaries in the return-stack window of the live DATA region just
\      before SNAPGO. The build only succeeds if those writes took (the fixture
\      dies 70 otherwise), so a green build proves the window was non-zero. The
\      persisted image must then read back all zeros there, proving
\      SND-ZERO-RSTK cleared every stale return-stack frame.
\
\   2. Fail-closed on a failed final close.
\      test/snapshot-writer-close-fail.f arms SNAP-CLOSE-SEAM so the snapshot
\      output descriptor is closed early; SNAP-WRITE-BYTES then observes the
\      failing close-rc and must die 74 "snap: output close failed" rather than
\      accept the half-written image.
\
\   3. Warm startup preserves the protected-WID registry captured in DATA. The
\      real snapshot must retain more than the two reserved entries, and its
\      live slot 2 must reject publication on both batch input paths.
\
\   4. imgdump locates the real snapshot trailer only from the target header.
\      Corrupting that one locator in a copy must report no-snapshot even though
\      the original trailer bytes remain in the file.
\
\ The snapshot source is emitted with BF-EMIT-SNAP-RUN-SOURCE-WITH, which inserts
\ the fixture after the builder tail and before the snap driver. Both fixtures
\ live above SNAP-TAIL-MARK, so SNAP-RETIRE-GO forgets them before the image is
\ written; nothing reaches a shipped snapshot.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/vector.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-fork.f
require lib/source.f
require lib/build.f
require lib/codesign.f
require lib/content-key.f
require lib/date.f
require tools/build-fixpoint.f
require lib/test.f

package SNAP-WRITER-TEST

$8000 constant CAP
240000 constant TIMEOUT-MS
74 constant CLOSE-FAIL-RC
ENGINE-ERROR:SEAL-PACKAGE constant FORGE-RC
create OUT CAP allot
create ERR CAP allot
variable OUT-U
variable ERR-U
variable RC
variable EXITED

: ENGINE$ ( -- ptr u8 n )
   s" HABU_UNDER_TEST" GETENV dup 0 > if exit then
   2drop s" bin/hb" ;

\ ---- isolated tmp root ----
create ROOT-BUF FS-PATH-CAP allot
variable ROOT-U

: ROOT ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: ROOT! ( ptr u8 n -- ) {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY
   u ROOT-U ! ;

: SETUP-ROOT ( -- )
   s" habu-snapshot-writer" TMPDIR-MKDIR ROOT!
   ROOT CLEANUP-TREE+
   ROOT BF-TMP! ;

: SNAP0$ ( -- ptr u8 n )
   s" hb-snap0" BF-A$ ;

: SNAP-SRC$ ( -- ptr u8 n )
   s" hb-snap-src" BF-B$ ;

: BAD-SNAP$ ( -- ptr u8 n )
   s" hb-snap-bad" BF-A$ ;

: CLEAN-SNAP0 ( -- )
   SNAP0$ 2dup EXISTS? if REMOVE-FILE else 2drop then ;

\ ---- snapshot image reader ----
create IMGP 8 allot
variable IMGU
variable TR-LAST
variable NZ

: IMG ( -- ptr u8 )
   IMGP @ ;

: LOAD-IMAGE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u FILE-SIZE {: sz:n :}
   sz MEM-ALLOC-BYTES drop IMGP !
   a u IMG sz READ-ALL IMGU !
   IMGU @ sz <> if s" snapshot writer short read" 74 die then ;

: U32@ ( n -- n ) {: k:n :}
   IMG k + c@
   IMG k 1+ + c@ 8 lshift or
   IMG k 2 + + c@ 16 lshift or
   IMG k 3 + + c@ 24 lshift or ;

: MAGIC-AT? ( n -- bool ) {: at:n :}
   s" !SNAPSBH" {: mp:ptr mu:n :}
   0 mu 0 ?do
      IMG at i + + c@ mp i + c@ = if 1+ then
   loop mu = ;

: LAST-TRAILER ( -- n )
   -1 TR-LAST !
   IMGU @ 8 - 0 ?do
      i MAGIC-AT? if i TR-LAST ! then
   loop
   TR-LAST @ dup 0 < if s" snapshot writer trailer magic missing" 74 die then ;

: DATA-OFF ( -- n )
   LAST-TRAILER {: tr:n :}
   tr tr 32 + U32@ - ;

: RSTK-NONZERO ( -- n )
   DATA-OFF {: base:n :}
   0 NZ !
   RSTK-END RSTK-OFF ?do
      base i + U32@ 0 <> if 1 NZ +! then
      base i + 4 + U32@ 0 <> if 1 NZ +! then
   8 +loop
   NZ @ ;

\ ---- child snapshot build with rc + stderr capture ----
: BUILD-ARGV ( -- )
   PROC-ARGV-RESET
   PROC-ENV-RESET
   s" HB_TMP" >LEN ROOT >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --build" >LEN PROC-ARGV+
   SNAP-SRC$ >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   ROOT >LEN PROC-ARGV+ ;

: CAPTURE! ( result<pcap:captured,pcap:failed> -- )
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  0 RC ! ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  c RC>N RC ! ENDOF
   ;MATCH ;

: BUILD-CAPTURE ( -- )
   ENGINE$ >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE CAPTURE! ;

: BUILD-WITH ( ptr u8 n -- ) {: ia:ptr iu:n :}
   CLEAN-SNAP0
   s" hb-snap-src" ia iu s" src/habu/snap.f" BF-EMIT-SNAP-RUN-SOURCE-WITH
   BUILD-ARGV
   BUILD-CAPTURE ;

: ERR$ ( -- ptr u8 n )
   ERR ERR-U @ ;

: IMG-ARGV ( ptr u8 n -- ) {: path:ptr pathu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" tools/imgdump.f" >LEN PROC-ARGV+
   s" --" >LEN PROC-ARGV+
   s" --snap" >LEN PROC-ARGV+
   path pathu >LEN PROC-ARGV+ ;

: IMG-CAPTURE ( ptr u8 n -- )
   IMG-ARGV
   ENGINE$ >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-CAPTURE CAPTURE! ;

: ASSERT-SNAPSHOT ( ptr u8 n -- )
   IMG-CAPTURE
   RC @ 0 T=
   ERR-U @ 0 T=
   OUT OUT-U @ s" ndict " CONTAINS? TTRUE ;

: ASSERT-NO-SNAPSHOT ( ptr u8 n -- )
   IMG-CAPTURE
   RC @ 0 T=
   ERR-U @ 0 T=
   OUT OUT-U @ TRIM s" no-snapshot" STR= TTRUE ;

: WRITE-BAD-SNAPSHOT ( -- )
   8 0 ?do
      0 IMG IMAGE-TEXT-SIZE-OFF i + + c!
   loop
   BAD-SNAP$ IMG IMGU @ WRITE-ALL ;

\ ---- warm snapshot probes ----
: STORE! ( len len outcome -- )
   MATCH outcome
     exited OF RC ! 0 0= EXITED ! ENDOF
     signaled OF RC ! 0 0= 0= EXITED ! ENDOF
     timeout OF 0 RC ! 0 0= 0= EXITED ! ENDOF
   ;MATCH
   LEN>N ERR-U !  LEN>N OUT-U ! ;

: WARM-LOAD ( ptr u8 n -- ) {: s:ptr su:n :}
   SNAP-SRC$ s su WRITE-ALL
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   SNAP-SRC$ >LEN PROC-ARGV+
   SNAP0$ >LEN  OUT 0 >LEN  OUT CAP >LEN  ERR CAP >LEN  TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME STORE! ;

: WARM-STDIN ( ptr u8 n -- ) {: s:ptr su:n :}
   PROC-ARGV-RESET
   SNAP0$ >LEN  s su >LEN  OUT CAP >LEN  ERR CAP >LEN  TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME STORE! ;

: PARSE-OUT ( -- n )
   OUT OUT-U @ TRIM STR>NUMBER? MATCH option
     some OF ENDOF
     none OF T-FAIL 0 ENDOF
   ;MATCH ;

: ASSERT-REJECT ( -- )
   EXITED @ TTRUE
   RC @ FORGE-RC T=
   ERR$ s" hb: cannot publish into protected word" CONTAINS? TTRUE ;

: PROBE-N$ ( -- ptr u8 n )
   s" data-base PROT-WID-N-CELL + @ . " ;

: FORGE-E2$ ( -- ptr u8 n )
   s" data-base PROT-WID-OFF + 8 + @ $FFFFFFFF and set-current : FOO ( -- n ) 1 ;" ;

: WARM-CASE ( -- )
   s" warm snapshot retains protected WIDs" T-LABEL
   PROBE-N$ WARM-LOAD
   EXITED @ TTRUE  RC @ 0 T=
   PARSE-OUT dup 2 > TTRUE
   s" warm live protected-WID count matches serialized snapshot DATA" T-LABEL
   DATA-OFF PROT-WID-N-CELL + U32@ T=
   s" warm protected slot 2 rejects publication (--load)" T-LABEL
   FORGE-E2$ WARM-LOAD  ASSERT-REJECT
   s" warm protected slot 2 rejects publication (stdin)" T-LABEL
   FORGE-E2$ WARM-STDIN  ASSERT-REJECT ;

\ ---- scenarios ----
: POISON-CASE ( -- )
   s" test/snapshot-writer-poison.f" BUILD-WITH
   s" poisoned snapshot builds (canaries planted and proven live)" T-LABEL
   RC @ 0 T=
   SNAP0$ EXISTS? TTRUE
   SNAP0$ LOAD-IMAGE
   s" snapshot zeros the persisted return-stack window" T-LABEL
   RSTK-NONZERO 0 T=
   WARM-CASE
   s" imgdump accepts the production snapshot" T-LABEL
   SNAP0$ ASSERT-SNAPSHOT
   WRITE-BAD-SNAPSHOT
   s" imgdump rejects a corrupted header-owned trailer locator" T-LABEL
   BAD-SNAP$ ASSERT-NO-SNAPSHOT ;

: CLOSE-FAIL-CASE ( -- )
   s" test/snapshot-writer-close-fail.f" BUILD-WITH
   s" snapshot writer fails closed when the final close fails" T-LABEL
   RC @ CLOSE-FAIL-RC T=
   ERR$ s" snap: output close failed" CONTAINS? TTRUE ;

: BODY ( -- )
   SETUP-ROOT
   POISON-CASE
   CLOSE-FAIL-CASE ;

public

: RUN ( -- )
   T-RESET
   CLEANUP-RESET
   [: BODY ;] catch {: code:n :}
   CLEANUP-RUN
   code 0 <> if code throw then
   T-REPORT
   s" snapshot-writer-test: ok" type cr ;

;package

SNAP-WRITER-TEST:RUN

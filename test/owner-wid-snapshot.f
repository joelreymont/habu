\ owner-wid-snapshot.f - adversarial proofs for the snapshot image writer.
\
\ Two properties of src/habu/snap-lib.f, each driven through a real snapshot
\ build in a child process (SNAPGO forgets its definitions and exits, so the
\ writer path can only be exercised out of process):
\
\   1. Return-stack zeroing. test/owner-wid-snapshot-poison.f plants fixed
\      non-zero canaries in the return-stack window of the live DATA region just
\      before SNAPGO. The build only succeeds if those writes took (the fixture
\      dies 70 otherwise), so a green build proves the window was non-zero. The
\      persisted image must then read back all zeros there, proving
\      SND-ZERO-RSTK cleared every stale return-stack frame.
\
\   2. Fail-closed on a failed final close.
\      test/owner-wid-snapshot-close-fail.f arms SNAP-CLOSE-SEAM so the snapshot
\      output descriptor is closed early; SNAP-WRITE-BYTES then observes the
\      failing close-rc and must die 74 "snap: output close failed" rather than
\      accept the half-written image.
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
require tools/date.f
require tools/build-fixpoint.f
require lib/test.f

package OWNER-WID-SNAPSHOT

$8000 constant CAP
240000 constant TIMEOUT-MS
74 constant CLOSE-FAIL-RC
create OUT CAP allot
create ERR CAP allot
variable OUT-U
variable ERR-U
variable RC

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
   s" habu-owner-wid-snapshot" TMPDIR-MKDIR ROOT!
   ROOT CLEANUP-TREE+
   ROOT BF-TMP! ;

: SNAP0$ ( -- ptr u8 n )
   s" hb-snap0" BF-A$ ;

: SNAP-SRC$ ( -- ptr u8 n )
   s" hb-snap-src" BF-B$ ;

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
   IMGU @ sz <> if s" owner-WID snapshot short read" 74 die then ;

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
   TR-LAST @ dup 0 < if s" owner-WID snapshot trailer magic missing" 74 die then ;

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

: BUILD-CAPTURE ( -- )
   ENGINE$ >LEN
   OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS
   RUN-ARGV-ENV-CAPTURE
   MATCH result
     ok  OF PCAP-CAPTURED:UNMAKE {: o:len e:len :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  0 RC ! ENDOF
     err OF PCAP-FAILED:UNMAKE {: o:len e:len c:rc :}
            o LEN>N OUT-U !  e LEN>N ERR-U !  c RC>N RC ! ENDOF
   ;MATCH ;

: BUILD-WITH ( ptr u8 n -- ) {: ia:ptr iu:n :}
   CLEAN-SNAP0
   s" hb-snap-src" ia iu s" src/habu/snap.f" BF-EMIT-SNAP-RUN-SOURCE-WITH
   BUILD-ARGV
   BUILD-CAPTURE ;

: ERR$ ( -- ptr u8 n )
   ERR ERR-U @ ;

\ ---- scenarios ----
: POISON-CASE ( -- )
   s" test/owner-wid-snapshot-poison.f" BUILD-WITH
   s" poisoned snapshot builds (canaries planted and proven live)" T-LABEL
   RC @ 0 T=
   SNAP0$ EXISTS? TTRUE
   SNAP0$ LOAD-IMAGE
   s" snapshot zeros the persisted return-stack window" T-LABEL
   RSTK-NONZERO 0 T= ;

: CLOSE-FAIL-CASE ( -- )
   s" test/owner-wid-snapshot-close-fail.f" BUILD-WITH
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
   s" owner-wid-snapshot-test: ok" type cr ;

;package

OWNER-WID-SNAPSHOT:RUN

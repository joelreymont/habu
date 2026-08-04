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
\      registry is the wid-indexed bitmap described in src/habu/layout.f, so the
\      persisted band is read through that shape: its tag cell must carry
\      PROT-REG-TAG, bit 0 must be clear (wid 0 is not a wordlist), and it must
\      hold more than the two wordlists that are protected by rule. A wid whose
\      bit is set in the PERSISTED band must still be a member live after a warm
\      start, and publishing into it must be refused on both batch input paths.
\
\   4. A corrupt persisted registry is refused at snapshot-read. Both invalid
\      shapes the loader checks (EM-SNAPSHOT-VALIDATE-WIDS in src/habu/habu2.f)
\      are staged here by doctoring the real image: a tag cell that is not
\      PROT-REG-TAG, and a band with bit 0 set. Each must exit 79 with the named
\      diagnostic instead of restoring a registry it cannot trust. (This is the
\      snapshot band; the AOT band's forge is unreachable from its builder and is
\      owned by dot habu-forge-a-corrupt-844064a9.)
\
\   5. imgdump locates the real snapshot trailer only from the target header.
\      Corrupting that one locator in a copy must report no-snapshot even though
\      the original trailer bytes remain in the file.
\
\ The snapshot source is emitted with BF-EMIT-SNAP-RUN-SOURCE-WITH, which inserts
\ the fixture after the builder tail and before the snap driver. Both fixtures
\ live above SNAP-TAIL-MARK, so SNAP-RETIRE-GO forgets them before the image is
\ written; nothing reaches a shipped snapshot.

require lib/errors.f
require lib/string.f
require lib/fmt.f
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
79 constant SNAP-BAD-RC          \ EM-SNAPSHOT-RESTORE's corrupt-image exit
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

: BAD-BAND-NAME$ ( -- ptr u8 n )
   s" hb-snap-badband" ;

: BAD-BAND$ ( -- ptr u8 n )
   BAD-BAND-NAME$ BF-A$ ;

: CLEAN-SNAP0 ( -- )
   SNAP0$ 2dup EXISTS? if REMOVE-FILE else 2drop then ;

\ `--build` writes an UNSIGNED image: signing belongs to whoever installs it, so
\ the pipeline signs at install and every fixture that runs a build product signs
\ its own copy (tools/build-fixpoint-test.f BFT-SNAP0-BUILD does the same). On
\ macOS an unsigned image is SIGKILLed at exec before the loader runs, so the warm
\ probes below cannot start without this.
: SIGN-SNAP0 ( -- )
   s" hb-snap0" BF-CODESIGN-FORCE-TMP
   s" hb-snap0" BF-CHMOD-X-TMP ;

\ ---- snapshot image reader ----
create IMGP 8 allot
variable IMGU
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

: U64@ ( n -- n ) {: k:n :}
   0
   8 0 ?do
      IMG k i + + c@ i 8 * lshift or
   loop ;

: U8@ ( n -- n ) {: k:n :}
   IMG k + c@ ;

: U8! ( n n -- ) {: k:n v:n :}
   v IMG k + c! ;

\ The trailer sits at the end of the authenticated text extent; src/habu/layout.f
\ owns its size and field offsets, and the target header owns its location.
: TRAILER-OFF ( -- n )
   IMAGE-TEXT-SIZE-OFF U64@ IMAGE-TEXT-TRAILER-ADJ + SNAP-TRL-BYTES - ;

: DATA-OFF ( -- n )
   TRAILER-OFF {: tr:n :}
   tr tr SNAP-TRL-DATALEN + U64@ - ;

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

\ ---- the persisted protected-WID bitmap ------------------------------------
\ Same shape and same rules as the engine's own read-only view
\ (tools/prot-wid-probe.f): bit w of the band at PROT-BITS-OFF is set exactly
\ when wordlist w is protected, and the two OWNER-API wordlists are protected by
\ rule on every boot path rather than by a bit.
variable BAND-TOTAL
variable BAND-WID

: BAND-TAG ( -- n )
   DATA-OFF PROT-REG-TAG-CELL + U64@ ;

: BAND-BIT? ( n -- bool ) {: wid:n :}
   DATA-OFF PROT-BITS-OFF + wid 8 / + U8@
   wid 7 and rshift 1 and 0= 0= ;

: BAND-MEMBER? ( n -- bool ) {: wid:n :}
   wid OWNER-API-PUB-WID = wid OWNER-API-PRI-WID = or if 0 0= exit then
   wid BAND-BIT? ;

: BAND-COUNT! ( -- )
   0 BAND-TOTAL !
   PROT-WID-MAX 0 ?do i BAND-MEMBER? if 1 BAND-TOTAL +! then loop ;

\ The lowest wordlist THIS IMAGE records as protected, skipping the two that
\ every boot path protects by rule. Membership for such a wid can only come from
\ the band the snapshot carried, which is what the warm probes below test.
: BAND-WID! ( -- )
   0 BAND-WID !
   PROT-WID-MAX FIRST-DYNAMIC-WID ?do
      BAND-WID @ 0= i BAND-BIT? and if i BAND-WID ! then
   loop ;

\ ---- warm probe sources ----------------------------------------------------
: PROBE-COUNT$ ( -- ptr u8 n )
   s" require tools/prot-wid-probe.f PROT-WID-PROBE:COUNT . " ;

: PROBE-MEMBER$ ( n -- ptr u8 n ) {: wid:n :}
   SB-RESET
   s" require tools/prot-wid-probe.f : PRB ( -- ) " SB-APPEND
   wid FMT:SB-U
   s"  PROT-WID-PROBE:MEMBER? if 1 else 0 then . ; PRB" SB-APPEND
   SB$ ;

: FORGE$ ( n -- ptr u8 n ) {: wid:n :}
   SB-RESET
   wid FMT:SB-U
   s"  set-current : FOO ( -- n ) 1 ;" SB-APPEND
   SB$ ;

\ ---- doctored-band legs ----------------------------------------------------
: WRITE-BAND-COPY ( -- )
   BAD-BAND$ IMG IMGU @ WRITE-ALL
   BAD-BAND-NAME$ BF-CODESIGN-FORCE-TMP
   BAD-BAND-NAME$ BF-CHMOD-X-TMP ;

: RUN-BAND-COPY ( -- )
   PROC-ARGV-RESET
   BAD-BAND$ >LEN  OUT 0 >LEN  OUT CAP >LEN  ERR CAP >LEN  TIMEOUT-MS >MS
   RUN-ARGV-STDIN-CAPTURE-OUTCOME STORE! ;

\ Doctor one byte of the persisted image, run the result, restore the byte so the
\ later imgdump probes still see the image the writer produced.
: DOCTOR-BYTE ( n n -- ) {: off:n val:n :}
   off U8@ {: orig:n :}
   off val U8!
   WRITE-BAND-COPY
   RUN-BAND-COPY
   off orig U8! ;

: DOCTOR-TAG ( -- )
   DATA-OFF PROT-REG-TAG-CELL + 0 DOCTOR-BYTE ;

: DOCTOR-WID0 ( -- )
   DATA-OFF PROT-BITS-OFF + {: off:n :}
   off  off U8@ 1 or  DOCTOR-BYTE ;

: ASSERT-BAND-REFUSED ( -- )
   EXITED @ TTRUE
   RC @ SNAP-BAD-RC T=
   ERR$ s" hb: snapshot trailer corrupt" CONTAINS? TTRUE ;

: WARM-CASE ( -- )
   BAND-COUNT!
   BAND-WID!
   s" persisted protected-WID band carries the bitmap shape tag" T-LABEL
   BAND-TAG PROT-REG-TAG T=
   s" persisted band leaves wid 0 clear" T-LABEL
   0 BAND-BIT? TFALSE
   s" persisted band holds more than the two wordlists protected by rule" T-LABEL
   BAND-TOTAL @ 2 > TTRUE
   s" persisted band names a wordlist protected by the image alone" T-LABEL
   BAND-WID @ 0 > TTRUE
   \ The live count can only exceed the persisted one: loading the probe's own
   \ packages may protect further wordlists in the warm process, and nothing may
   \ ever drop a member the image carried.
   s" warm start restores at least the persisted band" T-LABEL
   PROBE-COUNT$ WARM-LOAD
   EXITED @ TTRUE  RC @ 0 T=
   PARSE-OUT BAND-TOTAL @ >= TTRUE
   s" a wordlist the image protected is still a live member" T-LABEL
   BAND-WID @ PROBE-MEMBER$ WARM-LOAD
   EXITED @ TTRUE  RC @ 0 T=
   PARSE-OUT 1 T=
   s" warm protected wordlist rejects publication (--load)" T-LABEL
   BAND-WID @ FORGE$ WARM-LOAD  ASSERT-REJECT
   s" warm protected wordlist rejects publication (stdin)" T-LABEL
   BAND-WID @ FORGE$ WARM-STDIN  ASSERT-REJECT
   s" band with a wrong shape tag is refused at snapshot-read" T-LABEL
   DOCTOR-TAG ASSERT-BAND-REFUSED
   s" band claiming wid 0 is refused at snapshot-read" T-LABEL
   DOCTOR-WID0 ASSERT-BAND-REFUSED ;

\ ---- scenarios ----
: POISON-CASE ( -- )
   s" test/snapshot-writer-poison.f" BUILD-WITH
   s" poisoned snapshot builds (canaries planted and proven live)" T-LABEL
   RC @ 0 T=
   SNAP0$ EXISTS? TTRUE
   SIGN-SNAP0
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

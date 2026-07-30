\ owner-wid-child.f - build and run the cold owner proof in a child process.

require test/owner-wid-guard.f
OWNER-WID-GUARD:REQUIRE-HARNESS

require test/owner-wid-image.f
require test/owner-wid-doctor.f
require lib/test.f

package OWNER-WID-CHILD

$4000 constant CAP
120000 constant TIMEOUT-MS
10000 constant MODE-TIMEOUT-MS
0 constant KIND-EXITED
1 constant KIND-SIGNALED
2 constant KIND-TIMEOUT
create OUT CAP allot
create ERR CAP allot
variable OUT-U
variable ERR-U
variable RUN-KIND
variable RUN-CODE

: RUN-KIND! ( outcome -- )
   MATCH outcome
     exited OF RUN-CODE ! KIND-EXITED RUN-KIND ! ENDOF
     signaled OF RUN-CODE ! KIND-SIGNALED RUN-KIND ! ENDOF
     timeout OF 0 RUN-CODE ! KIND-TIMEOUT RUN-KIND ! ENDOF
   ;MATCH ;

: OUT$ ( -- ptr u8 n )
   OUT OUT-U @ ;

: ERR$ ( -- ptr u8 n )
   ERR ERR-U @ ;

: KIND$ ( -- ptr u8 n )
   RUN-KIND @ KIND-TIMEOUT = if s" timeout" exit then
   s" signal" ;

: DIAG-CMD ( ptr u8 n ptr u8 n -- )
   {: engine:ptr engineu:n file:ptr fileu:n :}
   s" owner-wid-child: abnormal run-file: " type
   engine engineu type
   s"  --load " type file fileu type cr
   s" owner-wid-child: outcome " type KIND$ type
   s"  code " type RUN-CODE @ .
   s" timeout-ms " type TIMEOUT-MS . cr ;

: DIAG-STREAM ( ptr u8 n ptr u8 n -- ) {: label:ptr labelu:n a:ptr u:n :}
   s" owner-wid-child: " type label labelu type
   s"  bytes " type u .
   s" cap " type CAP . cr
   a u type cr ;

: RUN-DIAG ( ptr u8 n ptr u8 n -- )
   {: engine:ptr engineu:n file:ptr fileu:n :}
   engine engineu file fileu DIAG-CMD
   s" stdout" OUT$ DIAG-STREAM
   s" stderr" ERR$ DIAG-STREAM ;

: RUN-PREP ( -- )
   PROC-ARGV-RESET
   PROC-ENV-RESET
   s" HABU_OWNER_WID_HARNESS" >LEN s" 1" >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;

: RUN-RESULT ( len len outcome -- n )
   RUN-KIND!
   LEN>N ERR-U !
   LEN>N OUT-U !
   RUN-KIND @ KIND-EXITED = if RUN-CODE @ exit then
   -1 ;

: RUN-CAPTURE ( ptr u8 n n -- n )
   {: engine:ptr engineu:n timeout:n :}
   engine engineu >LEN
   OUT CAP >LEN ERR CAP >LEN timeout >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME
   RUN-RESULT ;

: RUN-STDIN-CAPTURE ( ptr u8 n ptr u8 n n -- n )
   {: engine:ptr engineu:n in:ptr inu:n timeout:n :}
   engine engineu >LEN in inu >LEN
   OUT CAP >LEN ERR CAP >LEN timeout >MS
   RUN-ARGV-ENV-STDIN-CAPTURE-OUTCOME
   RUN-RESULT ;

\ Timeboxed child run: the capture poll is bounded by TIMEOUT-MS; a timeout or
\ signal death reports a named diagnostic and returns -1 so the caller's exact
\ rc assertion fails instead of an unattributed E-PROC-TIMEOUT throw.
: RUN-FILE-MS ( ptr u8 n ptr u8 n n -- n )
   {: engine:ptr engineu:n file:ptr fileu:n timeout:n :}
   RUN-PREP
   s" --load" >LEN PROC-ARGV+
   file fileu >LEN PROC-ARGV+
   engine engineu timeout RUN-CAPTURE {: code:n :}
   code 0 < if
      engine engineu file fileu RUN-DIAG
   then
   code ;

: RUN-FILE ( ptr u8 n ptr u8 n -- n )
   TIMEOUT-MS RUN-FILE-MS ;

: RUN-SOURCE ( ptr u8 n ptr u8 n -- n )
   {: engine:ptr engineu:n file:ptr fileu:n :}
   RUN-PREP
   file fileu >LEN PROC-ARGV+
   engine engineu MODE-TIMEOUT-MS RUN-CAPTURE {: code:n :}
   code 0 < if
      engine engineu file fileu RUN-DIAG
   then
   code ;

: RUN-STDIN ( ptr u8 n ptr u8 n -- n )
   {: engine:ptr engineu:n in:ptr inu:n :}
   RUN-PREP
   engine engineu in inu MODE-TIMEOUT-MS RUN-STDIN-CAPTURE {: code:n :}
   code 0 < if
      s" owner-wid-child: abnormal warm stdin mode" type cr
   then
   code ;

: ASSERT-OK-OUT ( n ptr u8 n -- )
   {: code:n msg:ptr msgu:n :}
   code 0 T=
   OUT$ msg msgu CONTAINS? TTRUE
   ERR-U @ 0 T= ;

: ASSERT-MODES ( ptr u8 n -- )
   {: engine:ptr engineu:n :}
   engine engineu s" test/owner-wid-state.f" MODE-TIMEOUT-MS RUN-FILE-MS
   s" owner-wid-state-test: ok" ASSERT-OK-OUT
   engine engineu s" test/owner-wid-state.f" RUN-SOURCE
   s" owner-wid-state-test: ok" ASSERT-OK-OUT
   engine engineu s" test/owner-wid-eval.f" MODE-TIMEOUT-MS RUN-FILE-MS
   s" owner-wid-eval-test: ok" ASSERT-OK-OUT
   engine engineu s" 271828 . cr" RUN-STDIN
   s" 271828" ASSERT-OK-OUT ;

: ASSERT-STATE ( ptr u8 n -- ) {: engine:ptr engineu:n :}
   engine engineu s" test/owner-wid-state.f" RUN-FILE {: code:n :}
   code 0 <> if engine engineu s" test/owner-wid-state.f" RUN-DIAG then
   code 0 T=
   OUT$ s" owner-wid-state-test: ok" CONTAINS? TTRUE
   ERR-U @ 0 T= ;

: ASSERT-HIDDEN ( ptr u8 n -- ) {: engine:ptr engineu:n :}
   engine engineu s" test/owner-wid-call.f" RUN-FILE 70 T=
   ERR$ s" owner-wid-add" CONTAINS? TTRUE ;

: ASSERT-PRIVATE-HIDDEN ( ptr u8 n -- ) {: engine:ptr engineu:n :}
   engine engineu s" test/owner-wid-private-call.f" RUN-FILE 70 T=
   ERR$ s" OWNER-WID-COLD-TEST:PRIVATE-PROOF" CONTAINS? TTRUE ;

: ASSERT-ROLE-SWAP ( ptr u8 n -- ) {: engine:ptr engineu:n :}
   engine engineu s" test/owner-wid-role-swap.f" RUN-FILE 70 T=
   ERR$ s" owner-row-idx" CONTAINS? TTRUE
   ERR$ s" prot-row-idx" CONTAINS? TTRUE ;

: ASSERT-BAD ( ptr u8 n n ptr u8 n -- )
   {: engine:ptr engineu:n code:n msg:ptr msgu:n :}
   engine engineu s" test/owner-wid-state.f" RUN-FILE code T=
   ERR$ msg msgu CONTAINS? TTRUE ;

: ASSERT-CAP ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: engine:ptr engineu:n file:ptr fileu:n msg:ptr msgu:n :}
   engine engineu file fileu RUN-SOURCE 77 T=
   ERR$ msg msgu CONTAINS? TTRUE ;

: ASSERT-GEN-CAP ( ptr u8 n ptr u8 n -- )
   {: engine:ptr engineu:n file:ptr fileu:n :}
   engine engineu file fileu RUN-SOURCE 67 T=
   ERR$ S\" hb: uncaught throw code 7169\n" T$= ;

: BODY ( -- )
   OWNER-WID-IMAGE:BUILD
   BUILD-EXT:ASSERT-EMPTY
   OWNER-WID-DOCTOR:BUILD
   OWNER-WID-IMAGE:AOT-HB$ ASSERT-STATE
   OWNER-WID-IMAGE:SNAP-HB$ ASSERT-STATE
   OWNER-WID-DOCTOR:SNAP-TYPE-OK$ ASSERT-STATE
   OWNER-WID-IMAGE:AOT-HB$ ASSERT-HIDDEN
   OWNER-WID-IMAGE:SNAP-HB$ ASSERT-HIDDEN
   OWNER-WID-IMAGE:AOT-HB$ ASSERT-PRIVATE-HIDDEN
   OWNER-WID-IMAGE:SNAP-HB$ ASSERT-PRIVATE-HIDDEN
   OWNER-WID-IMAGE:AOT-HB$ ASSERT-ROLE-SWAP
   OWNER-WID-IMAGE:SNAP-HB$ ASSERT-ROLE-SWAP
   OWNER-WID-DOCTOR:AOT-BAD$ 82 s" hb: AOT owner frame corrupt" ASSERT-BAD
   OWNER-WID-DOCTOR:AOT-MAL$ 82 s" hb: AOT owner frame corrupt" ASSERT-BAD
   OWNER-WID-DOCTOR:AOT-LIMIT$ 82 s" hb: AOT owner frame corrupt" ASSERT-BAD
   OWNER-WID-DOCTOR:SNAP-PAIR-CAP$ OWNER-WID-DOCTOR:QUAL-CAP-SRC$
      s" LIMIT-QUAL:WORD" ASSERT-CAP
   OWNER-WID-DOCTOR:SNAP-PAIR-CAP$ OWNER-WID-DOCTOR:PKG-CAP-SRC$
      s" LIMIT-PACKAGE" ASSERT-CAP
   OWNER-WID-DOCTOR:SNAP-WL-CAP$ OWNER-WID-DOCTOR:WL-CAP-SRC$
      s" hb: wordlist WID space exhausted" ASSERT-CAP
   OWNER-WID-DOCTOR:SNAP-PAIR-CAP$ OWNER-WID-DOCTOR:GEN-CAP-SRC$
      ASSERT-GEN-CAP
   OWNER-WID-DOCTOR:SNAP-OLD$ 80 s" hb: snapshot format version unsupported" ASSERT-BAD
   OWNER-WID-DOCTOR:SNAP-BAD$ 79 s" hb: snapshot trailer corrupt" ASSERT-BAD
   OWNER-WID-DOCTOR:SNAP-MAL$ 79 s" hb: snapshot trailer corrupt" ASSERT-BAD
   OWNER-WID-DOCTOR:SNAP-MAG$ 79 s" hb: snapshot trailer corrupt" ASSERT-BAD
   OWNER-WID-DOCTOR:SNAP-WID1$ 79 s" hb: snapshot trailer corrupt" ASSERT-BAD
   OWNER-WID-DOCTOR:SNAP-WID2$ 79 s" hb: snapshot trailer corrupt" ASSERT-BAD
   OWNER-WID-DOCTOR:SNAP-WID-HI$ 79 s" hb: snapshot trailer corrupt" ASSERT-BAD
   OWNER-WID-DOCTOR:SNAP-DUP$ 79 s" hb: snapshot trailer corrupt" ASSERT-BAD
   OWNER-WID-DOCTOR:SNAP-ZERO$ 79 s" hb: snapshot trailer corrupt" ASSERT-BAD
   OWNER-WID-DOCTOR:SNAP-RSVD$ 79 s" hb: snapshot trailer corrupt" ASSERT-BAD
   OWNER-WID-DOCTOR:SNAP-ALIAS$ 79 s" hb: snapshot trailer corrupt" ASSERT-BAD
   OWNER-WID-DOCTOR:SNAP-REUSE$ 79 s" hb: snapshot trailer corrupt" ASSERT-BAD
   OWNER-WID-DOCTOR:SNAP-PROT$ 79 s" hb: snapshot trailer corrupt" ASSERT-BAD
   OWNER-WID-DOCTOR:SNAP-XPTR$ 79 s" hb: snapshot trailer corrupt" ASSERT-BAD
   OWNER-WID-DOCTOR:SNAP-PTR$ 79 s" hb: snapshot trailer corrupt" ASSERT-BAD
   OWNER-WID-DOCTOR:SNAP-DICT-EXT$ 79 s" hb: snapshot trailer corrupt" ASSERT-BAD
   OWNER-WID-DOCTOR:SNAP-LEAD$ 79 s" hb: snapshot trailer corrupt" ASSERT-BAD
   OWNER-WID-DOCTOR:SNAP-TRAIL$ 79 s" hb: snapshot trailer corrupt" ASSERT-BAD
   OWNER-WID-DOCTOR:SNAP-DBL$ 79 s" hb: snapshot trailer corrupt" ASSERT-BAD
   OWNER-WID-DOCTOR:SNAP-TYPE-PRI$ 79 s" hb: snapshot trailer corrupt" ASSERT-BAD
   OWNER-WID-DOCTOR:SNAP-TYPE-ALIAS$ 79 s" hb: snapshot trailer corrupt" ASSERT-BAD
   OWNER-WID-DOCTOR:SNAP-TYPE-MISS$ 79 s" hb: snapshot trailer corrupt" ASSERT-BAD
   OWNER-WID-DOCTOR:SNAP-CROSS-DUP$ 79 s" hb: snapshot trailer corrupt" ASSERT-BAD
   OWNER-WID-DOCTOR:SNAP-TYPE-WIDN$ 79 s" hb: snapshot trailer corrupt" ASSERT-BAD
   OWNER-WID-DOCTOR:SNAP-LIVE$ ASSERT-MODES
   OWNER-WID-IMAGE:AOT-HB$ s" test/owner-wid-build-forge.f" RUN-FILE 70 T=
   ERR$ s" SET" CONTAINS? TTRUE ;

public

: RUN ( -- )
   T-RESET
   CLEANUP-RESET
   [: BODY ;] catch {: code:n :}
   CLEANUP-RUN
   code 0 <> if code throw then
   T-REPORT
   s" owner-wid-child-test: ok" type cr ;

;package

OWNER-WID-CHILD:RUN

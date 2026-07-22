\ candidate-validation-test.f - structural batching and boundary regression.

require lib/test.f
require lib/test/src-shape.f

package CANDIDATE-VALIDATION-TEST

34 constant CV-DQ                    \ ASCII double-quote closing an s" ..." path

variable CV-CUR                      \ byte cursor while scanning the loaded source
variable CV-N                        \ enumerated case-path count
variable CV-PA                       \ current path slice pointer
variable CV-PU                       \ current path slice length

: CV-PATH$ ( -- ptr u8 n )
   CV-PA @ CV-PU @ ;

\ Advance to the next case path with the requested prefix, setting CV-PATH$
\ to the text between the quotes; false once none remain. An unterminated path is a
\ broken source and dies loudly rather than yielding a truncated slice.
: CV-NEXT? ( ptr u8 n -- bool ) {: prefix:ptr prefixu:n :}
   SHAPE:TEXT {: a:ptr u:n :}
   a CV-CUR @ + u CV-CUR @ - prefix prefixu FIND-SUB MATCH option
     none OF STR-FALSE exit ENDOF
     some OF IDX>N ENDOF
   ;MATCH {: rel:n :}
   CV-CUR @ rel + 3 + {: ps:n :}
   a ps + u ps - CV-DQ INDEX-OF MATCH option
     none OF s" candidate-validation-test: unterminated case path" 1 die ENDOF
     some OF IDX>N ENDOF
   ;MATCH {: rc:n :}
   a ps + CV-PA !  rc CV-PU !  ps rc + 1+ CV-CUR !  STR-TRUE ;

: CV-CASES ( ptr u8 n -- ) {: prefix:ptr prefixu:n :}
   0 CV-CUR !
   begin prefix prefixu CV-NEXT? while
      CV-PATH$ FILE? TTRUE
      CV-PATH$ 1 SHAPE:COUNT=
      CV-N @ 1+ CV-N !
   repeat ;

\ Whitebox contract for the case list: every declared case path resolves to a file
\ on disk and appears exactly once, and the path count equals the enumerated
\ `construct case-kind` rows, so a path with no kind (or a kind with no path) fails.
: CASES ( -- )
   s" test/candidate-validation.f" SHAPE:LOAD
   0 CV-N !
   S\" s\q test/" CV-CASES
   S\" s\q tools/" CV-CASES
   CV-N @
   s" construct case-kind positive"   SHAPE:COUNT
   s" construct case-kind diagnostic" SHAPE:COUNT +
   s" construct case-kind negative"   SHAPE:COUNT +
   T= ;

: WORKER ( -- )
   s" $40000 constant SRC-CAP" SHAPE:MUST-HAVE
   s" $8000 constant IO-CAP" SHAPE:MUST-HAVE
   s" 120000 constant TIMEOUT-MS" SHAPE:MUST-HAVE
   s" TIMEOUT-MS >MS SUBJECT:RUN STORE" SHAPE:MUST-HAVE
   s" RUN-ARGV" SHAPE:MUST-LACK
   s" ENGINE-ID:KEY$ TARGET-HEX HEX-LEN STR=" SHAPE:MUST-HAVE
   s" SRC SRC-U @ SRC-HEX HASH" SHAPE:MUST-HAVE
   s" OUT OUT-U @ OUT-HEX HASH" SHAPE:MUST-HAVE
   s" ERR ERR-U @ ERR-HEX HASH" SHAPE:MUST-HAVE
   s" ERR-U @ 0= path pathu" SHAPE:MUST-HAVE
   s" needle needleu ERR-HAS? path pathu" SHAPE:MUST-HAVE
   S\" 0 SCRIPT-ARGV$ s\q shared\q STR= if RUN-SHARED exit then" SHAPE:MUST-HAVE
   S\" 0 SCRIPT-ARGV$ s\q top-row\q STR= if RUN-TOP-ROW exit then" SHAPE:MUST-HAVE
   s" manifest" SHAPE:MUST-HAVE
   s" ENUM case-kind positive diagnostic negative ;ENUM" SHAPE:MUST-HAVE
   s" kind MATCH case-kind" SHAPE:MUST-HAVE
   S\" s\q test/top-row-hook-test.f\q construct case-kind positive 0 s\q \q s\q \q RUN-CASE" 1 SHAPE:COUNT=
   S\" s\q tools/hb-build-direct-lints-test.f\q construct case-kind positive 0 s\q \q s\q \q RUN-CASE" 1 SHAPE:COUNT= ;

: DIRECT-PIN ( ptr u8 n -- )
   1 SHAPE:COUNT= ;

: SB-N ( n -- ) {: n:n :}
   n 10 >= if n 10 / RECURSE then
   n 10 mod STR-ZERO + SB-APPEND-C ;

\ Assert the count declared beside the cases equals the live occurrences: require
\ the source to hold the LF-anchored line `<enumerated> constant NAME`. A row
\ deleted without adjusting its count changes the number, so the line vanishes.
: DECLARED= ( ptr u8 n ptr u8 n -- ) {: kind:ptr kindu:n name:ptr nameu:n :}
   kind kindu SHAPE:COUNT {: got:n :}
   SB-RESET STR-LF SB-APPEND-C got SB-N STR-SPACE SB-APPEND-C
   s" constant " SB-APPEND name nameu SB-APPEND
   SB$ SHAPE:MUST-HAVE ;

: MANIFESTS ( -- )
   s" construct case-kind positive"   s" N-POSITIVE"   DECLARED=
   s" construct case-kind diagnostic" s" N-DIAGNOSTIC" DECLARED=
   s" construct case-kind negative"   s" N-NEGATIVE"   DECLARED=
   S\" s\q shared\q MANIFEST\n   SHARED-CASES" SHAPE:MUST-HAVE
   S\" s\q top-row\q MANIFEST\n   TOP-ROW-CASE" SHAPE:MUST-HAVE ;

: RUNNER ( -- )
   s" test/gate-validation-worker.f" SHAPE:LOAD
   s" 12 constant NESTED-EXEC-MAX" SHAPE:MUST-HAVE
   s" count NESTED-EXEC-MAX > if" SHAPE:MUST-HAVE
   S\" candidate candidateu s\q shared\q s\q candidate validation shared\q RUN-WORKER" SHAPE:MUST-HAVE
   S\" s\q bin/hb\q s\q shared\q s\q baseline validation shared\q RUN-WORKER" SHAPE:MUST-HAVE
   S\" candidate candidateu s\q top-row\q s\q candidate validation top-row\q RUN-WORKER" SHAPE:MUST-HAVE
   S\" s\q bin/hb\q s\q top-row\q" SHAPE:MUST-LACK
   s" CHECK-BASELINE" 2 SHAPE:COUNT=
   s" CANDIDATE-EVIDENCE CANDIDATE-EVIDENCE-U @ type" SHAPE:MUST-HAVE ;

: BOUNDARIES ( -- )
   s" test/top-row-hook-test.f" SHAPE:LOAD
   S\" s\q 1 set-top-check\q TRH-BARE$ TRH-RUN-LOAD" DIRECT-PIN
   S\" s\q data-base 8 + set-top-check\q TRH-BARE$ TRH-RUN-LOAD" DIRECT-PIN
   S\" s\q set-top-check\q TRH-BARE$ TRH-RUN-LOAD" DIRECT-PIN
   s" MISSING-HOOK$ TRH-RUN-LOAD" DIRECT-PIN
   s" CUSTOM-HOOK$ TRH-RUN-LOAD" DIRECT-PIN
   s" TRH-CHILD-HOOK$ TRH-RUN-LOAD" DIRECT-PIN
   S\" s\q 1 set-top-check\q TRH-BARE$ TRH-RUN-STDIN" DIRECT-PIN
   s" TRH-CHILD-HOOK$ TRH-RUN-STDIN" DIRECT-PIN
   s" TRH-PREFLIGHT-REPLACE$ TRH-RUN-SUBJECT" DIRECT-PIN
   s" TRH-PREFLIGHT-FORGE$ TRH-RUN-SUBJECT" DIRECT-PIN
   s" TRH-SEAL-FORGE$ TRH-RUN-SUBJECT" DIRECT-PIN
   s" TRH-SNAP-SEAL-FORGE$ TRH-RUN-SUBJECT" DIRECT-PIN
   s" TRH-BELOW-FORGE$ TRH-RUN-SUBJECT" DIRECT-PIN
   s" TRH-ABOVE-FORGE$ TRH-RUN-SUBJECT" DIRECT-PIN
   s" TRH-PREFLIGHT-REINSTALL$ TRH-RUN-SUBJECT" DIRECT-PIN ;

public

: TEST ( -- )
   T-RESET
   CASES
   WORKER
   MANIFESTS
   RUNNER
   BOUNDARIES
   T-REPORT
   s" candidate-validation-test: ok" type cr ;

;package

CANDIDATE-VALIDATION-TEST:TEST

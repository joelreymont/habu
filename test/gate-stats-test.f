\ gate-stats-test.f - focused coverage for gate-stats.f.
\
\ Load after lib/test.f and test/gate-stats.f.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require test/gate-stats.f

create GST-ROOT-BUF FS-PATH-CAP allot
create GST-ROW-BUF GS-LINE-CAP allot
create GST-SAVE-BUF FS-PATH-CAP allot
create GST-CHILD-SAVE GS-LINE-CAP allot
variable GST-ROOT-U
variable GST-ROW-U
variable GST-SAVE-U
variable GST-CHILD-SAVE-U

: GST-COPY! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < if E-FS-PATH throw then
   u FS-PATH-CAP > if E-FS-PATH throw then
   a GST-ROOT-BUF u BYTE-COPY
   u GST-ROOT-U ! ;

: GST-ROOT$ ( -- ptr u8 n )
   GST-ROOT-BUF GST-ROOT-U @ ;

: GST-SAVE$ ( -- ptr u8 n )
   GST-SAVE-BUF GST-SAVE-U @ ;

: GST-GS-SAVE ( -- )
   GS-ENSURE
   GS-PATH$ {: a:ptr u:n :}
   u FS-PATH-CAP > if E-FS-PATH throw then
   a GST-SAVE-BUF u BYTE-COPY
   u GST-SAVE-U ! ;

: GST-GS-RESTORE ( -- )
   GST-SAVE$ GS-COPY-PATH!
   -1 GS-INITED ! ;

: GST-CHILD-SAVE! ( -- )
   GS-CHILD-U @ GS-LINE-CAP > if E-STR-CAPACITY throw then
   GS-CHILD-BUF GST-CHILD-SAVE GS-CHILD-U @ BYTE-COPY
   GS-CHILD-U @ GST-CHILD-SAVE-U ! ;

: GST-CHILD-RESTORE ( -- )
   GST-CHILD-SAVE GS-CHILD-BUF GST-CHILD-SAVE-U @ BYTE-COPY
   GST-CHILD-SAVE-U @ GS-CHILD-U ! ;

: GST-ROW-RESET ( -- )
   0 GST-ROW-U ! ;

: GST-ROW+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a GST-ROW-BUF GST-ROW-U @ + u BYTE-COPY
   GST-ROW-U @ u + GST-ROW-U ! ;

: GST-ROW-TAB ( -- )
   STR-TAB GST-ROW-BUF GST-ROW-U @ + c!
   GST-ROW-U @ 1+ GST-ROW-U ! ;

: GST-TEST-EXPECTED ( -- ptr u8 n )
   GST-ROW-RESET
   s" test" GST-ROW+ GST-ROW-TAB
   s" test phase" GST-ROW+ GST-ROW-TAB
   s" host-source" GST-ROW+ GST-ROW-TAB
   s" gate-runner" GST-ROW+ GST-ROW-TAB
   s" process" GST-ROW+ GST-ROW-TAB
   s" -" GST-ROW+
   GST-ROW-BUF GST-ROW-U @ ;

: GST-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-gate-stats" TMPDIR-MKDIR GST-COPY!
   GST-ROOT$ CLEANUP-TREE+
   GST-ROOT$ GS-ROOT! ;

: GST-WRITE-EVENTS ( -- )
   s" top-phase-spawn" GS-EVENT
   s" top-capture-spawn" GS-EVENT
   s" under-phase-spawn" GS-EVENT
   s" under-env" GS-EVENT
   s" runner-phase-spawn" GS-EVENT
   s" gate-runner-build" GS-EVENT
   s" inner-hb-spawn" GS-EVENT
   s" inner-hb-stdin" GS-EVENT
   s" inner-hb-stdin" GS-EVENT
   s" inprocess-eval" GS-EVENT
   s" boundary-test" GS-EVENT
   s" boundary-test" GS-EVENT
   s" maker-cache-hit" GS-EVENT
   s" maker-cache-miss" GS-EVENT
   s" maker-build" GS-EVENT
   s" maker-run" GS-EVENT
   s" artifact-cache-hit" GS-EVENT
   s" artifact-cache-miss" GS-EVENT
   s" candidate-build" GS-EVENT
   s" candidate-cache-hit" GS-EVENT
   s" candidate-cache-miss" GS-EVENT
   s" candidate-cache-install" GS-EVENT
   s" candidate-import" GS-EVENT
   s" candidate-ready" GS-EVENT
   s" candidate-build-skip" GS-EVENT
   s" candidate-validate" GS-EVENT
   s" candidate-cache-corrupt" GS-EVENT
   s" helper-spawn" GS-EVENT
   s" named helper" GS-HELPER-EVENT
   s" test phase" s" host-source" s" gate-runner" s" process" s" -" GS-TEST
   s" art phase" s" artifact" s" gate-runner" s" process" s" -" GS-TEST
   s" fast phase" 12 GS-SPAN
   s" slow phase" 34 GS-SPAN
   s" test phase" 20 GS-SPAN
   s" art phase" 5 GS-SPAN
   s" lib/demo-test.f" 7 GS-SPAN
   s" test/run-worker-stdlib.f" 4 GS-SPAN-LOAD
   GST-CHILD-SAVE!
   s" owned label" GS-CHILD-LABEL!
   s" owned label" 9 GS-SPAN
   s" free label" 8 GS-SPAN
   GST-CHILD-RESTORE ;

: GST-SCAN ( -- )
   GS-READ
   GS-SCAN ;

: GST-EXPECT-COUNTS ( -- )
   GS-TOP-PHASE @ 1 T=
   GS-TOP-CAPTURE @ 1 T=
   GS-UNDER-PHASE @ 1 T=
   GS-UNDER-ENV @ 1 T=
   GS-RUNNER-PHASE @ 1 T=
   GS-RUNNER-BUILD @ 1 T=
   GS-INNER-HB @ 1 T=
   GS-INNER-HB-STDIN @ 2 T=
   GS-INPROCESS-EVAL @ 1 T=
   GS-BOUNDARY @ 2 T=
   GS-MAKER-HIT @ 1 T=
   GS-MAKER-MISS @ 1 T=
   GS-MAKER-BUILD @ 1 T=
   GS-MAKER-RUN @ 1 T=
   GS-ARTIFACT-HIT @ 1 T=
   GS-ARTIFACT-MISS @ 1 T=
   GS-CANDIDATE @ 1 T=
   GS-CANDIDATE-HIT @ 1 T=
   GS-CANDIDATE-MISS @ 1 T=
   GS-CANDIDATE-INSTALL @ 1 T=
   GS-CANDIDATE-IMPORT @ 1 T=
   GS-CANDIDATE-READY @ 1 T=
   GS-CANDIDATE-BUILD-SKIP @ 1 T=
   GS-CANDIDATE-VALIDATE @ 1 T=
   GS-CANDIDATE-CORRUPT @ 1 T=
   GS-HELPER-SPAWN @ 2 T=
   GS-SPANS @ 6 T=
   GS-SLOW-MS @ 34 T=
   GS-SLOW-LABEL GS-SLOW-U @ s" slow phase" T$= ;

: GST-EXPECT-LOAD-SPANS ( -- )
   GS-LOAD-SPANS @ 1 T=
   GS-LOAD-MS @ 4 T= ;

: GST-EXPECT-SUBJECTS ( -- )
   0 GS-SUBJ-COUNT-PTR @ 1 T=
   0 GS-SUBJ-TOTAL-PTR @ 20 T=
   0 GS-SUBJ-MAX-PTR @ 20 T=
   3 GS-SUBJ-COUNT-PTR @ 1 T=
   3 GS-SUBJ-TOTAL-PTR @ 5 T=
   3 GS-SUBJ-MAX-PTR @ 5 T=
   1 GS-SUBJ-COUNT-PTR @ 0 T=
   2 GS-SUBJ-COUNT-PTR @ 0 T=
   GS-SPAN-STRAY @ 4 T=
   GS-SPAN-STRAY-EXPECTED @ 1 T=
   GS-SPAN-STRAY-UNEXPECTED @ 3 T=
   GS-LABEL-DUP @ 0 T= ;

: GST-EXPECT-DEDUPE ( -- )
   GS-BUF GS-U @ s" owned label" CONTAINS? TFALSE
   GS-BUF GS-U @ s" free label" CONTAINS? TTRUE ;

: GST-EXPECT-TEST ( -- )
   GS-BUF GS-U @ GST-TEST-EXPECTED CONTAINS? TTRUE ;

\ Two test rows share the label "dup label" but name DIFFERENT subjects
\ (host-source vs artifact); a span carries that label. Under raw string equality
\ GS-LABEL-SUBJ silently joined the span to whichever row indexed first
\ (host-source), inflating its total - the attribution miscount this dot fixes.
: GST-WRITE-DUP-EVENTS ( -- )
   s" dup label" s" host-source" s" gate-runner" s" process" s" -" GS-TEST
   s" dup label" s" artifact" s" gate-runner" s" process" s" -" GS-TEST
   s" uniq label" s" candidate-cli" s" gate-runner" s" process" s" -" GS-TEST
   s" dup label" 10 GS-SPAN
   s" uniq label" 7 GS-SPAN ;

: GST-EXPECT-LABEL-DUP ( -- )
   GS-LABEL-DUP @ 1 T=
   \ the ambiguous span is NOT mis-attributed to either colliding subject
   0 GS-SUBJ-COUNT-PTR @ 0 T=
   0 GS-SUBJ-TOTAL-PTR @ 0 T=
   3 GS-SUBJ-COUNT-PTR @ 0 T=
   \ it surfaces as an unexpected stray, and the unique-label span still attributes
   1 GS-SUBJ-COUNT-PTR @ 1 T=
   1 GS-SUBJ-TOTAL-PTR @ 7 T=
   GS-SPANS @ 2 T=
   GS-SPAN-STRAY @ 1 T=
   GS-SPAN-STRAY-UNEXPECTED @ 1 T= ;

: GST-TEST-LABEL-DUP ( -- )
   GST-ROOT$ GS-ROOT!
   GST-WRITE-DUP-EVENTS
   GST-SCAN
   GST-EXPECT-LABEL-DUP ;

: GST-TEST-SCAN ( -- )
   GST-PREPARE
   GS-PATH$ FILE? TTRUE
   GST-WRITE-EVENTS
   GST-SCAN
   GST-EXPECT-COUNTS
   GST-EXPECT-LOAD-SPANS
   GST-EXPECT-SUBJECTS
   GST-EXPECT-DEDUPE
   GST-EXPECT-TEST ;

: GST-MAIN-BODY ( -- )
   T-RESET
   GST-TEST-SCAN
   GST-TEST-LABEL-DUP
   CLEANUP-RUN
   GST-ROOT$ EXISTS? TFALSE
   T-REPORT
   s" gate-stats-test: ok" type cr ;

: GST-MAIN ( -- )
   GST-GS-SAVE
   [: GST-MAIN-BODY ;] catch {: rc:n :}
   GST-GS-RESTORE
   rc 0 <> if rc throw then ;

GST-MAIN

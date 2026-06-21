\ drive-process-test.f - focused tests for native stdlib process driver.

T-RESET

DTH-MODELS$ MR-REGISTRY!
s" fixture" MR-REQUIRE
s" -- bool" DS-SIG!
s" process" DS-CATEGORY!
s" temp script -> -1" DS-TESTS!
s" test-seed" DS-SEED!
1 DS-TRIAL !
9 DS-TASK-ORDER !
2 DS-K !
1 DS-MAX-REPAIRS !
107 DS-ID !
s" PROC-RUN-RC-OK?" DS-NAME!
s" process rc fixture" DS-SPEC!
DTH-SRC-RESET
DTH-SRC-TASK-HEAD
s" PROC-FIX-RC-PATH$ RUN-RC 0= " DTH-SRC+
DTH-SRC-END DP-RUN-TEXT
LR-OUTCOME$ s" pass" T$=
LR-FIRST-CHECKER$ s" certified" T$=
LR-TESTS-PASSED @ -1 T=
s" arm" s" habu-stdlib-process" DTH-ROW-NEED-S
s" prompt_sha256" DTH-ROW-NEED-KEY
s" final_bundle_sha256" DTH-ROW-NEED-KEY
CLEANUP-RUN

DTH-MODELS$ MR-REGISTRY!
s" fixture" MR-REQUIRE
s" -- bool" DS-SIG!
s" process" DS-CATEGORY!
s" temp script -> -1" DS-TESTS!
s" test-seed" DS-SEED!
1 DS-TRIAL !
9 DS-TASK-ORDER !
2 DS-K !
1 DS-MAX-REPAIRS !
108 DS-ID !
s" PROC-CAPTURE-OUTERR-OK?" DS-NAME!
s" process capture fixture" DS-SPEC!
DTH-SRC-RESET
DTH-SRC-TASK-HEAD
s" PROC-FIX-CAPTURE-PATH$ " DTH-SRC+
s" PROC-FIX-OUT PROC-FIX-CAP " DTH-SRC+
s" PROC-FIX-ERR PROC-FIX-CAP " DTH-SRC+
s" PROC-FIX-TIMEOUT-MS RUN-CAPTURE " DTH-SRC+
s" {: outu erru rc :} " DTH-SRC+
s" rc 0 <> if STR-FALSE exit then " DTH-SRC+
s" outu 3 <> if STR-FALSE exit then " DTH-SRC+
s" erru 3 <> if STR-FALSE exit then " DTH-SRC+
s" PROC-FIX-OUT outu PROC-FIX-OUT-WANT$ STR= 0= if STR-FALSE exit then " DTH-SRC+
s" PROC-FIX-ERR erru PROC-FIX-ERR-WANT$ STR= " DTH-SRC+
DTH-SRC-END DP-RUN-TEXT
LR-OUTCOME$ s" pass" T$=
LR-FIRST-CHECKER$ s" certified" T$=
LR-TESTS-PASSED @ -1 T=
s" arm" s" habu-stdlib-process" DTH-ROW-NEED-S
s" prompt_sha256" DTH-ROW-NEED-KEY
s" final_bundle_sha256" DTH-ROW-NEED-KEY
CLEANUP-RUN

DTH-MODELS$ MR-REGISTRY!
s" fixture" MR-REQUIRE
s" -- bool" DS-SIG!
s" process" DS-CATEGORY!
s" temp script -> -1" DS-TESTS!
s" test-seed" DS-SEED!
1 DS-TRIAL !
9 DS-TASK-ORDER !
2 DS-K !
1 DS-MAX-REPAIRS !
109 DS-ID !
s" PROC-CAPTURE-NONZERO-OK?" DS-NAME!
s" process nonzero fixture" DS-SPEC!
DTH-SRC-RESET
DTH-SRC-TASK-HEAD
s" PROC-FIX-NONZERO-PATH$ " DTH-SRC+
s" PROC-FIX-OUT PROC-FIX-CAP " DTH-SRC+
s" PROC-FIX-ERR PROC-FIX-CAP " DTH-SRC+
s" PROC-FIX-TIMEOUT-MS RUN-CAPTURE " DTH-SRC+
s" {: outu erru rc :} " DTH-SRC+
s" rc 7 = outu 0= and erru 0= and " DTH-SRC+
DTH-SRC-END DP-RUN-TEXT
LR-OUTCOME$ s" pass" T$=
LR-FIRST-CHECKER$ s" certified" T$=
LR-TESTS-PASSED @ -1 T=
s" arm" s" habu-stdlib-process" DTH-ROW-NEED-S
s" prompt_sha256" DTH-ROW-NEED-KEY
s" final_bundle_sha256" DTH-ROW-NEED-KEY
CLEANUP-RUN

DTH-MODELS$ MR-REGISTRY!
s" fixture" MR-REQUIRE
s" -- bool" DS-SIG!
s" process" DS-CATEGORY!
s" temp script -> -1" DS-TESTS!
s" test-seed" DS-SEED!
1 DS-TRIAL !
9 DS-TASK-ORDER !
2 DS-K !
1 DS-MAX-REPAIRS !
107 DS-ID !
s" PROC-RUN-RC-OK?" DS-NAME!
s" process rc fixture" DS-SPEC!
DTH-SRC-RESET
DTH-SRC-TASK-HEAD
s" -1 " DTH-SRC+
DTH-SRC-END DP-RUN-TEXT
LR-OUTCOME$ s" reject" T$=
LR-FIRST-CHECKER$ s" rejected" T$=
LR-TESTS-PASSED @ 0 T=
s" required stdlib word missing" DTH-ROW-HAS
CLEANUP-RUN

DTH-MODELS$ MR-REGISTRY!
s" fixture" MR-REQUIRE
s" -- bool" DS-SIG!
s" process" DS-CATEGORY!
s" temp script -> -1" DS-TESTS!
s" test-seed" DS-SEED!
1 DS-TRIAL !
9 DS-TASK-ORDER !
2 DS-K !
1 DS-MAX-REPAIRS !
107 DS-ID !
s" PROC-RUN-RC-OK?" DS-NAME!
s" process rc fixture" DS-SPEC!
DTH-SRC-RESET
DTH-SRC-TASK-HEAD
s" 0 SCRIPT-ARGV$ RUN-RC 0= " DTH-SRC+
DTH-SRC-END DP-RUN-TEXT
LR-OUTCOME$ s" reject" T$=
LR-FIRST-CHECKER$ s" rejected" T$=
LR-TESTS-PASSED @ 0 T=
s" forbidden fixture boundary" DTH-ROW-HAS
CLEANUP-RUN

DTH-MODELS$ MR-REGISTRY!
s" fixture" MR-REQUIRE
s" -- bool" DS-SIG!
s" process" DS-CATEGORY!
s" temp script -> -1" DS-TESTS!
s" test-seed" DS-SEED!
1 DS-TRIAL !
9 DS-TASK-ORDER !
2 DS-K !
1 DS-MAX-REPAIRS !
110 DS-ID !
s" PROC-CAPTURE-TIMEOUT" DS-NAME!
s" -- error" DS-SIG!
s" code E-PROC-TIMEOUT" DS-TESTS!
s" process timeout fixture" DS-SPEC!
DTH-SRC-RESET
s" : PROC-CAPTURE-TIMEOUT ( -- n n n ) " DTH-SRC+
s" PROC-FIX-HANG-PATH$ PROC-FIX-OUT PROC-FIX-CAP " DTH-SRC+
s" PROC-FIX-ERR PROC-FIX-CAP " DTH-SRC+
s" PROC-FIX-SHORT-TIMEOUT-MS RUN-CAPTURE " DTH-SRC+
DTH-SRC-END DP-RUN-TEXT
LR-OUTCOME$ s" reject" T$=
LR-FIRST-CHECKER$ s" rejected" T$=
LR-TESTS-PASSED @ 0 T=
s" code E-PROC-TIMEOUT" DTH-ROW-HAS
CLEANUP-RUN

DTH-MODELS$ MR-REGISTRY!
s" fixture" MR-REQUIRE
s" -- bool" DS-SIG!
s" process" DS-CATEGORY!
s" temp script -> -1" DS-TESTS!
s" test-seed" DS-SEED!
1 DS-TRIAL !
9 DS-TASK-ORDER !
2 DS-K !
1 DS-MAX-REPAIRS !
111 DS-ID !
s" PROC-CAPTURE-TRUNCATED" DS-NAME!
s" -- error" DS-SIG!
s" code E-PROC-TRUNCATED" DS-TESTS!
s" process truncation fixture" DS-SPEC!
DTH-SRC-RESET
s" : PROC-CAPTURE-TRUNCATED ( -- n n n ) " DTH-SRC+
s" PROC-FIX-LONG-PATH$ PROC-FIX-OUT PROC-FIX-SMALL-CAP " DTH-SRC+
s" PROC-FIX-ERR PROC-FIX-CAP " DTH-SRC+
s" PROC-FIX-TIMEOUT-MS RUN-CAPTURE " DTH-SRC+
DTH-SRC-END DP-RUN-TEXT
LR-OUTCOME$ s" reject" T$=
LR-FIRST-CHECKER$ s" rejected" T$=
LR-TESTS-PASSED @ 0 T=
s" code E-PROC-TRUNCATED" DTH-ROW-HAS
CLEANUP-RUN

T-REPORT
s" drive-process-test: ok" type cr

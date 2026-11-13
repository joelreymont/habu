---
title: Render every checker refusal before it throws
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T16:01:10.285236+03:00"
---
Found by lane 440 (d5c1a730, 7b67e9e1) and the batch-4d CHECKER-REFUSE census: checker refusals that throw a code above 255 without rendering a diagnostic reach the engine's reporter as 'hb: uncaught throw code N' and exit 67 instead of a located record and 70. Census on the batch-4d line: E-PACKAGE-NAME-CAP 7154 (a package or using name of 256+ bytes), E-USING-AMBIGUOUS 7144 (a bare token two used packages' publics both answer), E-CTOR-PROTECTED (package COLOUR after SUMTYPE colour), 7119 from PRODUCT ... DERIVE init on a record that is not fixed-cell, E-CHECKER-LAYOUT-BUFFER 7121, the cast codes 7129 and 7135, EXPORT 7113, the USING push 7136 (test/name-length-test.f USING-PUSH-IT), FIELD-ADD 7101, and the authority E-PKG-CONTEXT throws. Acceptance: each that source can reach renders a located record in prose and under --json-errors (shape from tools/diag-code.f) and raises through CHECKER-REFUSE, so --load and check.f exit 70; each that source cannot reach says why beside the throw; seen failing first through check.f and --load; base after batch 4d (refusalexit) lands.

Acceptance case 2026-11-10 (/home/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/castplace-review.md F3): a cast or linear refusal under --load renders its packet, exits 70, and still ends with 'hb: uncaught throw code 7197' (or 7129) on stderr, a line that is not JSON under DIAG-JSON. CAST-ERROR:RAISE throws the refusal's own code through CHECKER-REFUSE from the definer's compiled code and nothing on the load path catches it; the shared exit writes the line before it compares the code with REFUSAL-ABI:CODE-CELL (src/habu/main.f REPORT on x86-64; habu2.f on arm64, machinery being replaced, so there it is an acceptance case of the replacing interpreter). A refused colon definition throws 70 and ends with no such line. test/load-reject-diag-test.f ERR-LINE$ reads the first line of stderr only to step around it.


Merged from habu-name-e-pkg-d0d9674c (Name E-PACKAGE-NAME-CAP in its diagnostic), 2026-11-10:

Maki H3: E-PACKAGE-NAME-CAP 7154 (checker.f ~1149, ~11517) reaches the user as a bare 'uncaught throw code 7154'; name it with its message as generated-declaration.f ~226 does for constructors. Acceptance: a too-long package name prints the named error under the run and tools/check.f; a row fails before. Claim: carl.

---
title: Render every checker refusal before it throws
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T16:01:10.285236+03:00"
---
Found by lane 440 (d5c1a730, 7b67e9e1) and the batch-4d CHECKER-REFUSE census: checker refusals that throw a code above 255 without rendering a diagnostic reach the engine's reporter as 'hb: uncaught throw code N' and exit 67 instead of a located record and 70. Census on the batch-4d line: E-PACKAGE-NAME-CAP 7154 (a package or using name of 256+ bytes), E-USING-AMBIGUOUS 7144 (a bare token two used packages' publics both answer), E-CTOR-PROTECTED (package COLOUR after SUMTYPE colour), 7119 from PRODUCT ... DERIVE init on a record that is not fixed-cell, E-CHECKER-LAYOUT-BUFFER 7121, the cast codes 7129 and 7135, EXPORT 7113, the USING push 7136 (test/name-length-test.f USING-PUSH-IT), FIELD-ADD 7101, and the authority E-PKG-CONTEXT throws. Acceptance: each that source can reach renders a located record in prose and under --json-errors (shape from tools/diag-code.f) and raises through CHECKER-REFUSE, so --load and check.f exit 70; each that source cannot reach says why beside the throw; seen failing first through check.f and --load; base after batch 4d (refusalexit) lands.

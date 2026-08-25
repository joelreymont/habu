---
title: Pin or retire unpinnable row end-of-input arms
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T18:39:34.582202+02:00"
---

Full context: found while restoring print-comment lexing. In tools/lint/source-lex.f, FIELD-OPERAND's helpers each carry an end-of-input boolean (SKIP-PRINT, and the pre-existing SKIP-QUOTE, SKIP-ESC-QUOTE and PARSE-NEXT?) reporting ran past end of input. None of the four can be pinned by a fixture: ROW-BODY's own empty-field arm independently reaches the identical diagnostic at the identical site, so no input distinguishes them. Demonstrated by deleting each arm in turn and observing the 710-assertion suite stay green in all four cases — this is a pre-existing property of the row grammar, not a regression. Decide: either give ROW-BODY a distinguishable diagnostic per cause so each arm becomes pinnable, or delete the four booleans as defence-in-depth that no test can defend and let ROW-BODY own the condition alone. Do not leave four unpinnable guards in place with fixtures that appear to cover them. Acceptance: every remaining arm reds a specific named fixture when deleted, or is gone.

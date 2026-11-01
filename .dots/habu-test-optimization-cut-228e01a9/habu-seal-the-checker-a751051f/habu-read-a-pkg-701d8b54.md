---
title: "Read a package's parts in the source verifier"
status: active
priority: 2
issue-type: task
created-at: "2026-11-01T02:50:52.149230+02:00"
---

Lane pkg2a (L2 of the nested-package design): the verifier replay, its file scan and tools/check.f open, close and resolve a part as both loops do (RECORD-PACKAGE path, FILE-SCOPE-STEP depth, seal checks through CHECKER-RESOLVE:HEAD-ROW, CHK-PKG-OPEN, --verify-only buffer rule, neutral-scope reopen of an engine-held part). Brief /tmp/jerry-big/pkg2a-brief.md. Workspace .jj-ws.noindex/jerry-pkg2a on rnqmvxss (worker). Claim: jerry.

First worker read the code and handed off before editing; the lead settled its design (/tmp/jerry-big/pkg2a-design.md plus the brief's Design section: the path spelled from the open row inside the verifier window, from the mirror name outside it; no undefine entries). Second worker running.

Workers 3 and 4: PART-PATH refuses, in order, a part name too long for the composed path (E-PKG-CONTEXT, the engine's C-PACKAGE-PATH rule, by length alone), a length no name has (CK-NAME-SPAN?, 7154), then a colon. Probes r3k, c1, c4, m1t, m2, ex1t and probe4 answer as the engine on the candidate; lt1 and lt2 (over-long part names) now give the engine's E-PKG-CONTEXT. Lead's ruling on lt3, a top-level package token past CK-NAME-MAX holding a colon: the engine's reader names the colon (E-PKG-CONTEXT), the verifier the length (7154); the token has both faults and each answer names one, and the verifier keeps the checker's rule that no word reads a name past CK-NAME-MAX (test/name-length-test.f). No code change; a comment at PART-PATH says so. Worker 5 runs the rows, the whitebox rows and the defect-5 baseline (/tmp/jerry-big/pkg2a-handoff4.md).

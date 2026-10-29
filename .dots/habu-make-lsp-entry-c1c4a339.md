---
title: Make lsp-entry-test independent of wall time under load
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T01:46:51.074782+02:00"
---

Problem: tools/lsp-entry-test.f gives its child 30 s and the child does a cold verifier image build; at load average above 100 the child dies with E-PROC-TIMEOUT (-2502) at about 30.6 s on master's public engine, on the W^X W3 engine and on the Linux lane's engine (three landing gates 2026-10-30), leaving a partial img/check-verify build behind, and the suite row lsp-boundary reports kind=TIMEOUT-UNDER-LOAD while tools/lsp-test.f passes. Acceptance: the row's verdict does not depend on wall time under load: the cold verifier image build is shared setup built once outside the timed child (or the child's limit measures its own CPU), the 30 s ceiling is not raised, and the partial build is not left behind on a timeout; bin/hb --load tools/lsp-entry-test.f rc 0 at load average above 100 and the lsp-boundary row green in test/run.f. Files: tools/lsp-entry-test.f, test/run.f (row lsp-boundary). Verify: the test alone under load, then the suite row. Depends: none. Ownership: tools/lsp-entry-test.f. Claim: unassigned.

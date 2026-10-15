---
title: Share duplicated test fixtures
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:31.203872+03:00"
---

Problem: 145 groups of test fixture definitions are copied verbatim (about 68.6K chars, 36.6K of them in test/compiler). HB$ is defined 26 times while engine-candidate.f:53 PATH$ already exists; the c2 e2e helpers are copied 5 times; a PCAP helper is copied into at least 20 files. This is against docs/gate.md:235. Acceptance: HB$ copies use ENGINE-CANDIDATE PATH$; the c2 e2e helpers and the PCAP helper each live in one test library; other groups outside test/compiler shared where at least three files copy them; test/compiler left alone (tier 1). Files: test/*.f users and new shared test libraries. Verify: the rows of the touched tests. Depends: the unrun-tests dot (some copies are in files it deletes). Ownership: test files touched. Claim: unassigned.

Status (2026-10-16), landed on master:
- HB$ copies resolve through ENGINE-CANDIDATE: vnzynrmx, owxptlrs, lkwxmkvv.
- Shared fixtures and readers: lmmozozv (native-window child argv), znrxyukl (capture rows through LE), rmztkvxv, nloupynr, nvrxpovq (PCAP-RESULT), kmmqxson, utkwyruo, yvznoyrt (SUBJECT:EXITED?), qorouzuy (scratch-tree e2e), tmzmqqoz, onvnqukq, rwynskqw (native FILE e2e fixture).
- Brief A plrpnplw (SUBJECT:REFUSED?), B zpryzsns (c2 consumer rows), C owrxtwyt (NATIVE-FILE-E2E hb-build driver), D klkrwmxp (NATIVE-FILE-APP:BUILD-AT), E vwulyvnt (WINDOW-CHILD-ARGS:PAYLOAD), F pkykplvp (field boundary child keeps its TRY, with the reason), G zvxloyly, H uzpvxxyv, I xttoxksm (PCAP-RESULT:>N).
- Shared test harness: B12a yzqnllmt (eight plain-row suites), B12b mornvozr (twelve whitebox suites and checker-scan-index-lib.f), B12c xkkxmntt (their copies of T<>, TTRUE, TFALSE, TSNE, TDT-CONTAINS?), B12d vlvszqmz (six more plain-row suites), B12e mlrrrzrz (engine-suite.f, decl-replay-verify-source.f).

Remaining:
- Held for carl's chain, which owns or edits these files: test/c2-memory-e2e.f (the G2/G3 c2 e2e helper copies and its own CAPTURE-RESULT at line 63), test/native-file-unit-e2e.f (the G6 copies), and test/c2-xml-consumer-e2e.f, test/gate-images-test.f, test/native-file-admission-e2e.f.
- test/e2e-tree.f:30 keeps its own CAPTURE-RESULT.
- test/proc-pty.f keeps its harness: its T-FAIL dumps the terminal buffer (BUF-DUMP), and its parent row (test/runtime-regression-test.f GE-PROCESS-PTY) reads the success line "PASS: process/pty primitives". Moving it means changing both files together.

Dropped groups, each with its reason:
- test/c2-memory-scope-refusals.f EXITED?/REPORTED?: asserts stdout contains `said`; one file.
- test/structure-opaque-program.f:25 STATUS?: no stdout condition; one file.
- test/c2-diagnostics.f REFUSAL; test/c2-init-accessor-refusals.f REFUSED?: one file each.
- test/native-file-stripped-path-e2e.f BUILD: no --repl, per-call cache.
- test/aot-registry-identity.f, test/field-proj-boundary.f argv: code-span position and extra files.
- test/field-proj-boundary-child.f TRY: the child runs on the build window's core prefix, which has no prelude, so src/lib/test/eval.f cannot load (`E-UNDEFINED: 0<>`); a comment beside TRY says so.
- PCAP: address-cell-storage-oom.f, native-defer-image.f, capture-tree-test.f (different arms or one copy).
- `timeout OF false` arms outside SUBJECT probes stay: tools/check-verify-core.f:893, test/gate-signal-test.f:393, src/lib/process-tree-test.f:85, test/process-image-subject.f:102, test/gate-pool-orphan-test.f:69, test/check-signal-test.f:302; after A and B, four one-file SUBJECT probes still answer `false` on a deadline, against docs/gate.md:426-440.

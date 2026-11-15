---
title: Read the wid cells in the checker
status: closed
priority: 2
issue-type: task
created-at: "2026-11-04T20:30:03.643097+02:00"
closed-at: "2026-11-04T21:31:42.183957+02:00"
close-reason: "landed lmqysykl 4f9d012f: checker.f reads BOOT:NAMESPACE-WL and BOOT:GLOBAL-WL. tip batch (comment-only folds after it): native build rc 0; 21 focused rows green incl. test/native-window-owner.f, tools/build-fixpoint-test.f, test/cold-naming-test.f, field-proj-boundary, prim-owner-scope, ffi-callback, does-clause-record, ffi-cabi; wid probe bytes match; x86-64 cross-build rc 0; full suite 521/521 PASS (793 s). Reviewed (code-reviewer, every hunk and the lead's folds; H2/U1 refolded and re-reviewed)."
---

Problem: cross-build step 3d (/tmp/jerry-big/xbuild-order.md). While a build writes the next hb, the header cells BOOT:NAMESPACE-WL-CELL and BOOT:GLOBAL-WL-CELL name the product's wordlists; src/core/checker.f still spells the package marker as -1 (ten WL-PROBE arguments, two CK-REC-WID -1 = tests, -1 constant NAMESPACE and its two uses) and the global wordlist as 0 (three CK-REC-WID tests, three WL-PROBE arguments). Acceptance: each becomes BOOT:NAMESPACE-WL or BOOT:GLOBAL-WL (Joel 2026-11-04: every file calls the owner's reader in place of its own read); NAMESPACE goes; RETIRED stays (not a cell); comments naming DICT-WL:NAMESPACE, -1 name the reader instead. No new names; behaviour unchanged while the cells hold -1 and 0. Files: src/core/checker.f. Verify: native build of hb, test/package-seal.f, test/seal-package.f, test/using-test.f, test/replay-binding.f, test/protection-span.f, src/lib/task-test.f, the wid probe /tmp/jerry-boot2-tmp/probe.f, the x86-64 cross-build. Brief /tmp/jerry-big/ckwl-brief.md. Depends: habu-read-the-wid-ece6c281. Ownership: src/core/checker.f. Claim: agent=worker workspace=.jj-ws.noindex/jerry-ckwl.

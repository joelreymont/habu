---
title: Delete administrative scaffolding
status: closed
priority: 2
issue-type: task
created-at: "2026-08-02T18:58:35.447278+02:00"
close-reason: "Patch-equivalent result landed as 878d8cdab1bf."
---

Problem: three live mechanisms duplicate no product invariant: test/gate-engine-lib.f hard-codes a certification count that fails on every legitimate source definition change even though tools/build-fixpoint.f already fail-closes generated-source certification; tools/host-lint.f and its fixture enforce only absence of retired Python and seed-script spellings and TYPE-FIXES-PLAN item 40 sentences that resurrection lint; docs/nanogpt-inventory.md plus its three lint files are a status ledger protected by a status-ledger lint. Result: delete the census constants/functions/call and worker briefing bump rule while retaining VERIFY:CENSUS-COUNT, BF-CERTIFY-STAGE2, and the build census report; delete host-lint code/tests/enrollment/live docs and remove future host-lint obligations from open or active dots while preserving closed historical evidence; delete the nanoGPT inventory doc, lint code/tests, all gate/suite/conscious-exception edges, and stale prose links. Update AGENTS master gate to omit host-lint. No replacement lint, ledger, manifest, suite, compatibility path, or new dot. Ownership: administrative deletions and exact dangling-edge cleanup only. Acceptance: executable nonarchive references to deleted files or suite labels are zero; lint standalone floor remains correct; build fixpoint still certifies and reports; lint-tools, checked-boundary, test/run.f, Maki, and exact diff gates pass. Checkpoint: baseline host, nano inventory, build, and lint-tools gates are green; removing any file before its enrollment edges makes the real gate fail.

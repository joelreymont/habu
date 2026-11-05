---
title: Retire the whitebox build option with the old hb builder
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T23:20:37.924915+02:00"
---

Problem: the whitebox option of tools/native-build.f and tools/hb-build.f, CLASS-WHITEBOX (tools/native-build-core.f) and internal-mark.f's IMAGE-WHITEBOX/ENGINE-UNSEAL? branch build an unsealed engine. After the suite pruning no test uses it; docs/bootstrap.md 'Recovery routes' still builds a whitebox host when an older host cannot build a tree with a new baked checker API. DECISIONS 'the old builders' (2026-11-05): the old hb builder goes once the arm64 cross-build writes a working hb. Acceptance: with the old hb builder, the option, the class, the internal-mark branch and the bootstrap.md route go; a cross-built hb is the recovery route. Files: tools/native-build.f, tools/hb-build.f, tools/native-build-core.f, src/habu/internal-mark.f, docs/bootstrap.md. Verify: rg whitebox over src tools test docs prints only unrelated words; the cross-build recovery route builds hb. Depends: the arm64 cross-build writes a working hb (habu-link-a-build-fcce3685). Ownership: unassigned. Claim: unassigned.

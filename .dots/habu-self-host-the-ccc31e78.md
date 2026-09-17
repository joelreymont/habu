---
title: Self-host the x86_64 engine to a byte fixpoint
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T17:32:42.547997+03:00"
---

Problem: docs/x86-64.md (campaign habu-campaign-c6-targets-86bb56bb): the cross-built engine is the recovery artifact; the release artifact is the engine that rebuilds itself on the Intel machine to a byte fixpoint. Acceptance: the cross-built x86_64 engine runs tools/native-build.f on the Intel machine and the second generation is byte-identical to the first; test/run.f green on that engine; an AOT-stripped application built there runs; the x86_64 engine is a second release artifact with its own pin and hash, recorded in docs/bootstrap.md and docs/x86-64.md. Files: tools/native-build.f, docs/bootstrap.md, docs/x86-64.md. Verify: two generations compared with cmp on the Intel machine; test/run.f there. Depends: the cross-build, the SysV port. Ownership: Joel (x86_64 lane). Claim: unassigned.

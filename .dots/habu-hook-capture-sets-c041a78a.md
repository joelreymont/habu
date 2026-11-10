---
title: HOOK-CAPTURE sets HOOKED before its registration
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T12:21:34.720124+02:00"
---

Problem (code review, 2026-11-10): src/compiler/ir/context.f:518-520 HOOK-CAPTURE stores 1 into HOOKED and then calls IMAGE-LIFECYCLE:REGISTER, against the rule at src/lib/image-lifecycle.f PREPARE's comment (an owner sets its registered flag after REGISTER returns). A REGISTER that throws (HOOKS cannot grow) leaves HOOKED 1 with no hook, and that process never closes its session at capture. Acceptance: the store follows REGISTER. Files: src/compiler/ir/context.f. Verify: read; engine builds and src/lib/task-test.f passes. Depends: none. Ownership: that word. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-owned (commit ad52a02e, stacked on b46f9e3d).

---
title: Delete retired structure effects
status: closed
priority: 2
issue-type: task
created-at: "2026-08-02T19:27:08.202540+02:00"
close-reason: "Patch-equivalent result landed as 3a612dd9db82."
---

Why: src/core/structures-effects.f is an unloaded retired effect-row file; its only consumers are five absence/resurrection assertions in tools/bootstrap-codegen-test.f. Exact result: delete the file and those five assertions, with no replacement lint or absence test. Dependencies: none. Owned result: obsolete rows and resurrection scaffolding are absent. Package owner: none because no definition survives. Acceptance: repo-wide live search has zero structures-effects references except truthful historical archive text; bootstrap codegen and recovery remain green. Smallest owning check: bin/hb --load tools/bootstrap-codegen-test.f.


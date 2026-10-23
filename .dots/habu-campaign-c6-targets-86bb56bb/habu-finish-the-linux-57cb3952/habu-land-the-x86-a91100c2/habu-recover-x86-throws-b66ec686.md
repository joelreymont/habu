---
title: Recover x86 throws without guarded mutators
status: active
priority: 2
issue-type: task
created-at: "2026-10-22T13:12:42.397075+03:00"
---

Problem: after any task is live, an uncaught throw exits 79 on x86 (row oi-live-too-long expects 76): OUTER:EVAL-FRAMED-RANGE -> FRAME-RECOVER -> FRAME-ROLLBACK-CODE calls ndict! (and CODE-RECLAIM:TRUNCATE calls cp!), whose x86 bodies start with TASK-LIVE-GUARD, (kernel-x64.f ~3934). ARM habu2.f LEVALREC restores CP/NDICT/DP in assembly without the guard. Design first (Fable Plan): an internal trusted rollback primitive private to OUTER that skips only the task-live guard, vs moving this part of recovery into the x86 THROW. Acceptance: the oi-live-too-long reproducer exits 76 on pulsar; ARM unchanged; two-gen on both. Depends: none. Ownership: outer.f/definers.f recovery, kernel-x64.f, prims.f. Recipes: ~/.cache/tmp/jerry-c/lane-recipe.md (Mac/pulsar build+run), ~/.cache/tmp/jerry-c/lane-queue.md. Rule: master (ARM) behaviour and tests win; fix x86, never rewrite a master test. Claim: unassigned.

Pulsar update: Pulsar active: worker in .jj-ws.noindex/pulsar-intel-throws at intel 184663fe. Preserve guarded public mutators; add only the private trusted recovery operation needed by OUTER saved cursors. Acceptance includes original oi-live-too-long exit 76 and affected E2E recovery. Shared-code ARM checks remain OPEN for the Mac.

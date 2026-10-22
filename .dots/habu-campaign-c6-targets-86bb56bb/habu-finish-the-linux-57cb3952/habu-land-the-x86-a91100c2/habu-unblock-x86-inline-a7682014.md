---
title: Unblock x86 inline checked stores
status: open
priority: 2
issue-type: task
created-at: "2026-10-22T13:12:42.371288+03:00"
---

Problem: lane .jj-ws.noindex/jerry-x86-pstore (lyoswrvw WIP 'Inline checked stores on x86-64 tier 1', lane-handoff.md there; master row compiler-tier1-inline-prims 12/13/14/16) is blocked by (1) shared regalloc.f MB-FIXED-BITS letting a value sit in an op's fixed register declared by another form (100 3 / p ! stores garbage; probe ~/.cache/tmp/jerry-c/pulsar/p3.f) and (2) the cross capture taking the guard helper address from the host (rc 74). Next: Fable Plan with brief ~/.cache/tmp/jerry-c/briefs/plan-pstore.txt, then the allocator fix as its own commit landing first, then finish the lane; Fable reviews. Depends: none. Ownership: regalloc.f, lane files. Recipes: ~/.cache/tmp/jerry-c/lane-recipe.md (Mac/pulsar build+run), ~/.cache/tmp/jerry-c/lane-queue.md. Rule: master (ARM) behaviour and tests win; fix x86, never rewrite a master test. Claim: unassigned.

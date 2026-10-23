---
title: Review and land x86 profiler trim and reclaim
status: active
priority: 2
issue-type: task
created-at: "2026-10-22T13:12:42.320059+03:00"
---

Problem: ruypmwwq 82a1ca67 'Retire x86 profiler spans on trim and reclaim' (workspace .jj-ws.noindex/jerry-x86-proflate; src/habu/prof-x64.f TRIM-HELPER/RECLAIM-HELPER twins of habu2.f LPROFTRIM/LPROFRECLAIM, called from kernel-x64.f OCC-COUNT and DEF-CODE-RECLAIM-BODY) fixes x86 test/prof-rewind.f asserts 10,11,13; author: prof-rewind, x86-64-kernel-prof, prof-index, two-gen rc 0 on pulsar. Open: Fable review (register preservation at both sites, SIGALRM window, INDEX reuse vs C-PROF-INDEX-PRUNE, session-off no-op, ranges). Acceptance: ACCEPT, land. Verify: pulsar test/prof-rewind.f rc 0. Depends: none. Ownership: prof-x64.f. Recipes: ~/.cache/tmp/jerry-c/lane-recipe.md (Mac/pulsar build+run), ~/.cache/tmp/jerry-c/lane-queue.md. Rule: master (ARM) behaviour and tests win; fix x86, never rewrite a master test. Claim: unassigned.

Pulsar update: Pulsar active: independent read-only reviewer examines rebased ruypmwwq e65e0176 in .jj-ws.noindex/pulsar-intel-proflate; lead builds and runs focused profiler checks before intel landing. No master test changes authorized.

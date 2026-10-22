---
title: Review and land tier-neutral tier-0 cases
status: open
priority: 2
issue-type: task
created-at: "2026-10-22T13:12:42.339500+03:00"
---

Problem: puurwvnk ab459ed0 'Keep tier-0 cases tier-neutral' (workspace .jj-ws.noindex/jerry-x86-tier0; test/compiler/native-eval.f, gate-debug-lib.f, gate-dictionary-lib.f, outer-interpret.f, runtime-regression-test.f) selects tier-0 cases by tier@ with tier-1 twins asserting ARM '1 set-tier' output. Open: Opus review (brief ~/.cache/tmp/jerry-c/briefs/review-tier0.txt; build a fresh ARM engine from the tree — the jerry-land seed predates 90d909). Acceptance: ACCEPT, land; report test changes to Joel. Depends: none. Ownership: those tests. Recipes: ~/.cache/tmp/jerry-c/lane-recipe.md (Mac/pulsar build+run), ~/.cache/tmp/jerry-c/lane-queue.md. Rule: master (ARM) behaviour and tests win; fix x86, never rewrite a master test. Claim: unassigned.

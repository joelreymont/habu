---
title: Finish the x86 native body-reuse ABI
status: open
priority: 2
issue-type: task
created-at: "2026-10-22T13:12:42.358152+03:00"
---

Problem: rvmtmsrk 'WIP: Give x86-64 the native body-reuse ABI' (workspace .jj-ws.noindex/jerry-x86-bodyabi; design ~/.cache/tmp/jerry-c/bodyabi-design.md; state and next commands in lane-handoff-2.md there). All edits done; ARM build and two-gen ok. Open: x86 cross-build, pulsar self-host + two-gen, the design's proof rows on both hosts with hit counts, delete lane-handoff*.md, final describe, Fable review (ABI). Files: layout.f data-claims.f prims.f kernel-x64.f link-x64.f definers.f runtime-x64.f interpret.f tools/native-layout.f docs/x86-64.md. Depends: none. Ownership: lane files. Recipes: ~/.cache/tmp/jerry-c/lane-recipe.md (Mac/pulsar build+run), ~/.cache/tmp/jerry-c/lane-queue.md. Rule: master (ARM) behaviour and tests win; fix x86, never rewrite a master test. Claim: unassigned.

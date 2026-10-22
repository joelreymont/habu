---
title: Land the 64-slot elision mask
status: open
priority: 2
issue-type: task
created-at: "2026-10-22T13:12:42.330253+03:00"
---

Problem: suvtokpo 'Let the elision mask cover all 64 residency slots' (workspace .jj-ws.noindex/jerry-x86-gep64) fixes tier-1 -8611 E-A64RAV-DKEEP for 64-cell values (shared select.f/select-x64.f). Fable review: code ACCEPT; fold-ins already applied in the working copy, unproven: dead 'i DSLOT-MAX < if' guards dropped in DSAVE-XFER/DEXIT-XFER (both selectors), docs/forth.md 'Native widths' entry corrected (65-cell definitions are -8303 E-NELAB-ARITY; -6688 unreachable). Open: ARM native-build, test/runtime-regression-test.f, test/compiler/native-dstack-alias.f, native-identity-spill.f, x64-select.f, two-generation-build; Opus follow-up review of the fold-ins; land. No new test row: the x86 gate's tier-1 GE-P2-FRAME-WIDE (GEP64:MAKE) pins it. Depends: none. Ownership: select*.f. Recipes: ~/.cache/tmp/jerry-c/lane-recipe.md (Mac/pulsar build+run), ~/.cache/tmp/jerry-c/lane-queue.md. Rule: master (ARM) behaviour and tests win; fix x86, never rewrite a master test. Claim: unassigned.

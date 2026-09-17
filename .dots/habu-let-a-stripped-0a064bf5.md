---
title: Let a stripped image keep execution tokens in data
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T14:42:02.585652+03:00"
---

Problem: the stripped product image refuses persistent data that holds an execution token (a defer bound at load, a quotation stored through a setter, ' word ,) with hb-build's exit-70 refusal (src/habu/aot-lib.f:80), so Tender's CLI and server cannot ship stripped and run as --repl images instead, which keeps the JIT in the image and blocks MemoryDenyWriteExecute in their unit (aspen, 2026-09-17, /tmp/aspen-aot-server.md). The AOT already relocates address cells inside DATA and records xt offset rows for the window (src/habu/aot-file.f S-XTOFFS, XTOFF-WINDOW-TAG); a code pointer in persistent data is the same shape against the code-span base. Acceptance: a stripped image carries a persistent cell holding an xt as a relocatable code-span offset, recorded like the XTOFF rows and restored against the image's code base at startup; defer bindings and stored quotations survive the strip and execute; a cell pointing into the dictionary records rather than code is still refused by name; a regression under test/compiler builds and runs such an image; Tender's server/entry.f builds stripped; docs/aot.md (or the hb-build doc) states the rule. Files: src/habu/aot-lib.f, src/habu/aot-closure.f, src/habu/aot-capture.f, src/habu/aot-file.f, the stripped startup in src/habu/habu2.f, tools/hb-build*.f, docs. Verify: the regression; byte fixpoint; test/run.f; the Tender server build. Depends: habu-name-the-cell-740feb52. Ownership: AOT. Claim: unassigned.

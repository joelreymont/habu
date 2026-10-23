---
title: Land the x86-64 integration stack on master
status: open
priority: 2
issue-type: task
created-at: "2026-10-22T13:12:42.296655+03:00"
---

Problem: the Intel port lives in an unpushed integration stack in workspace .jj-ws.noindex/jerry-land (tip tslownok "Seal bracket-tick at tier 1", base 52c7636b; intel@origin = 20b75344, master@origin moves). Acceptance (Joel's goal): (A) macOS ARM64 at the final tip (stack + merge of current master): tools/two-generation-build.f prints 'ok gen 5 matches gen 4 byte for byte' and bin/hb --load test/run.f exits 0 with no 'red tests'; (B) jj git push moves master@origin to a commit containing that merge and intel@origin to the same tip; (C) on pulsar-ua-106 (Linux x86-64) the cross-built hb runs, a native build plus tools/two-generation-build.f prints 'ok gen 5 matches gen 4', and bin/hb --load test/run.f exits 0; every red there fixed at its source, re-gated on ARM, landed, and C rerun green on the final commit. Children are the remaining lanes and reds (last full pulsar probe: 159 red of 491, older stack). Recipes: ~/.cache/tmp/jerry-c/lane-recipe.md (Mac/pulsar build+run), ~/.cache/tmp/jerry-c/lane-queue.md. Rule: master (ARM) behaviour and tests win; fix x86, never rewrite a master test. Depends: children. Ownership: lead. Claim: unassigned.

Pulsar lead now works toward goal C from intel 184663fe: 485 rows, 97 red; every red predates the Mac lanes. First blocker compiler-images-build (7179 E-LIVE) owns child habu-fix-intel-compiler-fb1b32c0 and blocks 45 missing-image rows. Active workspaces use pulsar-intel-* under .jj-ws.noindex. Pulsar pushes intel and lane code only; master receives only dot updates. Shared-code ARM checks stay open for the Mac; pulsar never accesses it.

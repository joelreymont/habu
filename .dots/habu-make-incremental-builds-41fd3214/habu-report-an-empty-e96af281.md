---
title: Report an empty maker inputs record instead of an uncaught throw
status: closed
priority: 2
issue-type: task
created-at: "2026-10-27T18:38:07.961673+02:00"
closed-at: "2026-10-27T19:58:53.401991+02:00"
close-reason: "INREC vnynmmsm (clears the record before each maker run; no new error shape) landed on master d81d39e9; integrated proof: engine, two-generation, bootstrap check rc 0, suite 501/502 (engine-stack-machine red on master, dot ed0ee303)"
---

Lane INREC, workspace .jj-ws.noindex/carl-inrec, brief carl-dots/.out/carl-file/brief-inrec.md. Suite row hb-build-fixtures died "hb: uncaught throw code -8597" (E-NUNIT-PROFILE) in HBT-STRIPPED-IMAGE (tools/hb-build-test.f:410); candidate: tools/hb-build-lib.f HBB-INPUTS-READ on a size-0 <BF-A>/hb-aot-got.inputs, uncaught through HBB-AOT-SAVE-INPUTS-RUN/HBB-STORE-OBJECT. Not reproduced standalone.

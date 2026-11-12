---
title: Keep no code a message runs into in a cut
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T12:39:18.205581+02:00"
---

Problem: src/arch/arm64/link-kernel.f VISIT (:464) reaches the next node whenever a node's last word is not terminal, and a message node ends in text, so a reached message keeps whatever code is emitted after it; emission order is the only guard and nothing checks it (the start-up's LMMAPDATA/LBLRANGE texts, habu2.f:485-486, kept the profiler handler and report, 4.4 KB, in every stripped program until EMIT-RUNTIME-SECTIONS was reordered). Acceptance: the cut follows a fall-through only out of code; reordering EMIT-RUNTIME-SECTIONS does not change which code a stripped program keeps; a test shows a program that reaches no profiler word keeps none of its code. Depends: none. Claim: jerry, workspace .jj-ws.noindex/jerry-cut.

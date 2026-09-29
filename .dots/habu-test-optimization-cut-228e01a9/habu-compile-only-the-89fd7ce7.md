---
title: Compile only the code under test at tier 1
status: closed
priority: 1
issue-type: task
created-at: "2026-09-29T18:01:21.039423+02:00"
closed-at: "2026-09-29T20:10:10.127913+02:00"
close-reason: Integrated tier load order with explicit suite preloads; focused tier and entry-guard checks and pooled native gate passed 510/510.
---

Problem: 29 test/compiler/native-* rows put '1 set-tier' before every require, so lib/test.f and code-reading tools compile through the optimizing compiler; the 21 *-aot twins pay the same via test/compiler/aot-mode.f. Measured 2-10x per row with the line after the harness requires. Evidence: ~/.cache/tmp/kestrel-gate/test-review/L1-compiler-native.md finding 1 and cross-cutting (a). docs/gate.md:82 prescribes the expensive order. Acceptance: set-tier sits after harness/tool requires and before the code under test in each row (a library the row tests stays after it); aot-mode.f requires lib/test.f first; docs/gate.md states the rule; each row still passes and still fails on a tier-1 defect in its subject. Files/Ownership: the 29 rows named in the report, test/compiler/aot-mode.f, docs/gate.md. Base: 614ae0ba (row-split stack head, not yet on master). Verify: every touched row passes standalone (bin/hb --load <row file>); a mutation of one moved or rewritten assertion fails; report per-row seconds before and after. Depends: none. Claim: agent=kestrel/worker workspace=.jj-ws/habu-compile-only-the-89fd7ce7

Also (~/.cache/tmp/kestrel-gate/test-review/L2-compiler-other.md): test/compiler/aot-mode.f:11 twins recompile lib/test.f (native-match lib/process*) at tier 1; test/compiler/native-create-does.f:3 and codegen-tail-probe.f:4 set the tier before their requires; delete the compiler-native-create-does-aot row (exact duplicate).

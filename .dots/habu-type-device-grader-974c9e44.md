---
title: Type device grader verdicts
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T21:27:11.455972+02:00"
---

Claim: agent=device_grader_impl workspace=.jj-ws/habu-type-device-grader-974c9e44 (RELEASED 2026-08-21: workspace gone, no live lane - gc) machine=spark (owns device and grading verdict types, exit decoding, direct consumers, and focused tests)

maki/eval/device.f:46-66 and :193-266 models five unchecked-device outcomes as EVN-* integers and the checked grader as raw 0/1/2. ED-EXIT>VERDICT, ED-OUTCOME>VERDICT, GRADE-CANDIDATE, GRADE-NOCHECK-CANDIDATE, DEVICE-SCORE, and ablation consumers all pass n, allowing device faults, assembler failures, typed-wrong results, counts, and exit codes to mix. Define a device-verdict ENUM (emit-failed | assembly-failed | wrong | green | fault) and a grade-verdict ENUM (rejected | typed-wrong | green). Keep process exit bytes as a private wire boundary that decodes exhaustively to device-verdict; map device-verdict to grade-verdict with MATCH, and make scoring/ablation APIs consume the semantic enum. Unknown child exits remain fault explicitly, never a numeric default leaking outward. Preserve child isolation, CUDA fault containment, cleanup, score definitions, exit protocol, reports, and device goldens. Add checker negatives for exit-code/verdict/count/foreign-enum swaps; exhaustive tests cover every process outcome, known/unknown exits, checker reject, emit/assembly failures, wrong/fault/green device results, and score tallies. Measure JIT/DATA/CODELEN and grading overhead before/after. Files: maki/eval/device.f and direct compare/ablation/tests. Verify eval/device/ablation/Orin suites, Maki, typed-local diff, type/package/host/dot lints, and full native gate. Ownership: grader result domains and exit decode only.

---
title: "Give check.f's run stage the caller's environment"
status: open
priority: 3
issue-type: task
created-at: "2026-09-24T10:19:22.580735+03:00"
---

tools/check.f's run stage hands the checked program an empty environment. tools/check-core.f CHK-RUN-CAPTURE spawns it through lib/process-argv.f RUN-ARGV-CAPTURE, whose spawn-argv-io primitive execs with no environment on Linux (src/habu/habu1.f BSPAWNARGVIO, Linux arm: envp is a one-NULL array on the stack), so the program sees no PATH, HOME or HB_TMP. Measured on engine 997e8dbe: a probe calling lib/process-env.f FIND-EXECUTABLE for mkfifo prints `mkfifo NOT found` under `tools/check.f probe.f` and `mkfifo found` under `--load probe.f` with the same env line; lib/fs-mutate-test.f, once its preverify passes (eb578f15), dies in the run stage at its TOOL lookup (`required executable missing: mkfifo`, lib/fs-mutate-test.f:506, E-PROC-PATH, rc 70) while `--load lib/fs-mutate-test.f` runs it. Fix: CHK-RUN-CAPTURE spawns through RUN-ARGV-ENV-CAPTURE after PROC-ENV-INHERIT-MISSING, the shape the test pool (docs/gate.md) and tools/hb-build-test-lib.f use, so the run stage inherits check.f's own environment; the env-less spawn stays where a caller chose it (tools/seed.f, tools/imgdump-test.f, tools/examples-test.f). Acceptance: the probe prints `mkfifo found` under tools/check.f; tools/check.f lib/fs-mutate-test.f is rc 0 or refused by a finding of that file; a row in tools/check-test-lib.f runs a program that reads PATH through check.f. Files: tools/check-core.f, tools/check-test-lib.f. Verify: the two runs, tools/check-core.f on the tree. Depends: none. Ownership: hazel. Claim: unassigned.

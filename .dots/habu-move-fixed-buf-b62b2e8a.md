---
title: Move fixed-buffer process captures onto CMD
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T14:04:24.367478+01:00"
---

Problem: src/lib/process-argv.f RUN-ARGV-CAPTURE, RUN-ARGV-CAPTURE-OUTCOME, RUN-ARGV-STDIN-CAPTURE, RUN-ARGV-STDIN-CAPTURE-OUTCOME, PROC-SPAWN-ARGV-CAPTURE and PROC-SPAWN-ARGV-STDIN-CAPTURE capture a child's output into caller buffers of a fixed size (^u8 len pairs). A child whose output outgrows the buffer throws E-PROC-STDERR-CAPACITY, so whether a run passes depends on its environment: test/structure-decl-suite.f failed when TMPDIR's path made tools/check.f's stderr exceed its $4000 buffer. CMD:WITH-COMMAND (src/lib/process-command.f) captures into growing vectors. About 170 calls in 43 Habu files (process-argv-test 34, process-test 24, tools/hb-cli-contracts-test 8, tools/seed.f 2, test/browser 10 files, test/wasm 5, about 20 other tests), plus maki/test/kernel.f and zen/test/run.f. Acceptance: every Habu caller captures through CMD; the six words and their tests are deleted; tim and phil are told before the install that drops them. Depends: none. Claim: unassigned.

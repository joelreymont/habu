---
title: twelve process capture entry points
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:25.942492+02:00"
---

Problem: lib/process.f:654-672, process-argv.f:103-143, process-env.f:319-363, process-cwd.f:470-495 define twelve RUN-*-CAPTURE words differing only in the spawn primitive and result shape; below them PROC-POLL-CAPTURE vs -OUTCOME (434-447, one line), PROC-POLL-IO vs -OUTCOME (519-542), PROC-RUN-CAPTURE-LOOP vs -OUTCOME-LOOP (554-570), five PROC-SPAWN-*-CAPTURE bodies; the RC form is derivable from the outcome form (PROC-OUTCOME>RC 224). Consumers: RUN-ARGV-ENV-STDIN-CAPTURE 0 tools/3 tests; PROC-CWD:SPAWN-ARGV-ENV-CWD-IO 0; PROC-RUN-ARGV-ENV-IO-RC 0 anywhere; PROC-RUN-RC 0 outside tests. Names break the 2-3 word rule (60 lib words have 5+ parts). Acceptance: one capture driver taking a spawn quotation and an optional stdin span, returning the outcome; one adapter to result; the variants deleted; process tests green. Files: lib/process*.f. Verify: lib/process*-test.f. Depends: none. Ownership: process. Claim: unassigned.

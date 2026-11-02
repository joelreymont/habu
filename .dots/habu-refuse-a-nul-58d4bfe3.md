---
title: Refuse a NUL inside CMD argument and env rows
status: open
priority: 2
issue-type: task
created-at: "2026-11-03T09:41:56.206509+02:00"
---

Problem (env-dup review, 2026-11-03, unreproduced): CMD:ARG+ (src/lib/process-command.f:210-215) and CMD:ENV+'s value (:225-234) take any bytes, and PROC-ENV-CHECK-ENTRY (src/lib/process-env.f:168-171), which CMD:ENV-ENTRY+ calls, checks only that the entry is non-empty, does not start with '=' and holds one; PROC-ENV-CHECK-NAME refuses a NUL in a name but nothing refuses one elsewhere. execve's rows end at the first NUL, so the child silently gets a shorter argument or row: an entry 'A<NUL>B=c' reaches the child as 'A' with no '=', and a value 'b<NUL>c' as 'b'. Ruling (carl, protocol-invalid input in public words is a fix, decided): refuse a NUL anywhere in an argument, a name or a value with the error each word already throws for bad input (E-PROC-ENV for the env words; ARG+'s own code), never truncate. Acceptance: a failing case first in src/lib/process-command-test.f for each of ARG+, ENV+ value and ENV-ENTRY+ name and value, then the refusals; PROC-ENV-SET and the process-wide table get the same check where they take an entry. Files: src/lib/process-command.f, src/lib/process-env.f, their tests. Verify: bin/hb --load src/lib/process-command-test.f, bin/hb --load src/lib/process-env-test.f. Depends: habu-a-later-cmd-377637bf (same lines). Ownership: process-command.f, process-env.f. Claim: unassigned.

---
title: Agree on a top-level [package] between load and check
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T07:07:10.323108+01:00"
---

Problem: a program file that leaves cells (a top-level `[package]`, a bare `1`) loads with rc 0 as `hb prog.f`, but tools/check.f rejects it with E-EVAL-RESIDUE, on the installed hb: the program file runs open. Acceptance: a program file that leaves cells gets one E-EVAL-RESIDUE record, the same from the load and check.f; a test pins it. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w197.

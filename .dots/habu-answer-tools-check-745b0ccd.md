---
title: Answer tools/check.f on a sealed engine file
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T13:37:06.925796+02:00"
---

Problem: on the sealed engine, bin/hb --load tools/check.f -- --all-errors src/habu/main.f answers 'E-STATEMENT-THROW main.f:17:9: throw 84 at ENGINE-MAIN' (SEAL-PACKAGE: layout.f's ENGINE-MAIN is sealed), neither a check result nor E-ENGINE-PROVIDED; ENGINE-PROVIDES? is false for main.f on arm64 because the arm64 engine never loads it (lane tp2, /tmp/jerry-big/bvr-answer.md). Acceptance: check.f names why it cannot check the file (a sealed package the file reopens) with a stable code, or checks it; a row that shows the answer. Files: tools/check.f and per root cause. Verify: the row. Depends: none. Ownership: per root cause. Claim: unassigned.

`--load` says less still: a file that requires src/lib/c2-owner.f and then opens `package C2-MEM` prints only `C2-MEM` and exits 84 on the public hb b1dc6a80, with no code or reason. The answer for the sealed reopen belongs to the package layer both front ends share, so check.f and --load name it alike.

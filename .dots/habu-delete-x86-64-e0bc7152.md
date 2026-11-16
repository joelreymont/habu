---
title: "Delete x86-64's copies of ?dup and int-mark"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T15:50:25.161353+01:00"
---

Problem: ?dup and int-mark are deleted from the arm64 engine, but src/habu/kernel-x64.f keeps QDUP-BODY and INT-MARK, (with the BINTMARK mention at ~:2449) and test/x86-64-kernel-atomics.f :248-249 calls int-mark and min-in-mark rows. Acceptance: those words and rows are gone and the x86-64 engine builds, when x86-64 work resumes. Depends: x86-64 is set aside until arm64 and Wasm are done. Claim: unassigned.

---
title: "Bring C4's kernel throws to the x86-64 kernel"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T10:25:04.147460+01:00"
closed-at: "2026-11-27T14:48:18.484177+01:00"
---

Problem: the arm64 kernel stops throw named codes (-9880..-9883, src/lib/error.f E-DATA-SPACE, E-UNDERFLOW, E-OVERLAY-OPEN, E-BODY-TEXT) and its catch-and-rethrow primitives keep a passed record (src/habu/habu1.f BRETHROW, PASSED-TO-CELL). src/habu/kernel-x64.f still names the deleted ENGINE-ERROR:OVERLAY-OPEN (:1527), so the x86-64 kernel does not build; its stops print their own lines (OVERLAY-OPEN$ :274) and its EVAL-CLOSED, FINALLY, C2-INVOKE and SOURCE-FILE-RUN twins rethrow without moving PASSED-TO-CELL. Acceptance: the x86-64 kernel builds; each stop throws its arm64 code; a throw out of evaluate-closed or finally around a text keeps the inner record, as on arm64. Depends: none. Claim: unassigned.

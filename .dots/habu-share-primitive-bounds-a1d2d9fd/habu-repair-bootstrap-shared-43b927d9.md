---
title: Repair bootstrap shared-guard parity
status: active
priority: 2
issue-type: task
created-at: "2026-07-18T10:24:19.839793+02:00"
---

Own bootstrap/cg/forth.fs, tools/bootstrap.sh, a new test/bootstrap-seal-owner-src.f fixture, and required FILEMAP registration. Replace the colliding PROT-GUARD vocabulary with a CALL-SPAN word in the existing GUARD vocabulary; route all eight stage0 callers through it; add OWNER-REG-OFF/OWNER-REG-LEN to both bootstrap protection guards; add a raw Gforth recovery gate proving an ordinary write succeeds and a sealed owner-registry write exits 83. Preserve register ABI and recovery-only host boundary. Acceptance: Gforth load smoke, bootstrap check-only gate, structural guard counts, filemap/host lint, exact diff review.

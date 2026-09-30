---
title: Run bin/hb --load on the cross-built engine (M4)
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.749146+03:00"
blocks:
  - habu-run-a-captured-15728fcc
  - habu-boot-the-arm64-d0d4421a
  - habu-keep-the-x86-6967d3cf
  - habu-emit-the-x86-e49a3447
---

Problem: the x86 engine has not yet run the Habu interpreter; milestone M4 joins lanes X and I.
Acceptance: the cross-built `hb-x64` runs the native-build SMOKE program (`: X ( -- n ) 42 ; X . cr`) and `test/tier.f`-class files at tier 1 on the ThinkPad; `docs/bootstrap.md` records the x86 build route (cross-build from spark, then self-build on the ThinkPad; no cold route, no Gforth mirror; recovery = cross-build from a working arm64 engine) and the two-command recipe (scp, run).
Files: a smoke driver under `test/x86-64-*`, `docs/bootstrap.md`.
Verify: spark cross-builds `hb-x64`; ThinkPad runs the SMOKE program and a `test/tier.f`-class file at tier 1.
Depends: habu-run-a-captured-15728fcc (X5), habu-boot-the-arm64-d0d4421a (I10c), habu-emit-x86-control-9a35e3b3 (K7), habu-add-sysv-ffi-17a130a1 (K11a).
Route: Alder (shared: docs/bootstrap.md).
Ownership: krait (Intel lane).
Claim: unassigned.

Lead note (2026-09-30, from P3's design): depends on K8c `habu-keep-the-x86-6967d3cf`. It is P3's native proof: its first tier-1 `:`…`;` through NCOMP and NPUB on x86 runs the commit path (`code-publish`, `callmap-set`, `xref-retarget`, `does-record`) and the slot rule. Acceptance: the `test/tier.f`-class run includes a call between two definitions and one `does>` definition.

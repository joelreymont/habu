---
title: Hook tier-0 compilation into the Habu loop
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.663677+03:00"
blocks:
  - habu-close-definitions-with-8ace78d8
---

Problem: B1 (chosen): the ARM64 product keeps tier 0, so the Habu compile loop needs the JIT, but the `LCOMPILE` dispatch's J-* handlers end in `lmainlbl B,` (`habu2.f:5005-5060`), so Habu cannot call it.
Acceptance: rows `jit-open`/`jit-token`/`jit-close`; the `LCOMPILE` dispatch returns through a `RET` stub (~100 lines plus the `forth.fs` mirror); x86 bodies refuse (`set-tier 0` refused on x86); the Habu compile loop at tier 0 hands each token to `jit-token`; ARM64 default tier and gate rows unchanged; `test/tier.f` green through the Habu loop.
Files: `src/habu/habu2.f`, `bootstrap/cg/forth.fs`, `src/habu/prims.f`, `src/habu/outer.f`.
Verify: spark `bin/hb --load test/tier.f` through the Habu loop; gate; the periodic no-binary check (`docs/bootstrap.md:206-220`).
Depends: habu-close-definitions-with-8ace78d8 (I5e). Serialise on `habu2.f` with K4, I10c and X7.
Route: Alder (shared: src/habu/habu2.f, bootstrap/cg/forth.fs, src/habu/prims.f, src/habu/outer.f).
Ownership: krait (Intel lane).
Claim: unassigned.

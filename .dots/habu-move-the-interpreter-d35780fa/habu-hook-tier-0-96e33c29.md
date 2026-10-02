---
title: Hook tier-0 compilation into the Habu loop
status: closed
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.663677+03:00"
closed-at: "2026-10-02T13:08:58.910733+03:00"
close-reason: "jit-open and jit-token rows run tier 0 from the Habu loop: test/outer-interpret.f (177 cases agree, tier-0 bodies, pass 2, refusals, nested jit-token), test/engine-writers.f, PTY REPL recovery, x86 boot tier 1 (hb-x64-kernel-tier 0, tier-zero 70), Gforth no-binary check OK; gen1=gen2=gen3 80031128; jit-close dropped because `;` closes through jit-token"
---

Problem: B1 (chosen): the ARM64 product keeps tier 0, so the Habu compile loop needs the JIT, but the `LCOMPILE` dispatch's J-* handlers end in `lmainlbl B,` (`habu2.f:5005-5060`), so Habu cannot call it.
Acceptance: rows `jit-open`/`jit-token`/`jit-close`; the `LCOMPILE` dispatch returns through a `RET` stub (~100 lines plus the `forth.fs` mirror); x86 bodies refuse (`set-tier 0` refused on x86); the Habu compile loop at tier 0 hands each token to `jit-token`; ARM64 default tier and gate rows unchanged; `test/tier.f` green through the Habu loop.
Files: `src/habu/habu2.f`, `bootstrap/cg/forth.fs`, `src/habu/prims.f`, `src/habu/outer.f`.
Verify: spark `bin/hb --load test/tier.f` through the Habu loop; gate; the periodic no-binary check (`docs/bootstrap.md:206-220`).
Depends: habu-close-definitions-with-8ace78d8 (I5e). Serialise on `habu2.f` with K4, I10c and X7.
Route: Alder (shared: src/habu/habu2.f, bootstrap/cg/forth.fs, src/habu/prims.f, src/habu/outer.f).
Ownership: krait (Intel lane).
Claim: unassigned.

Lead note (2026-09-30, from the I8/I5a design): `jit-open` does the JIT half of the head: the P2-nesting refusal (rc 76, `habu2.f:7825-7827`), the resets at `7843-7861`, EXECUTABLE-JIT-GUARD, FRAME-CELL and the link-save (`2799-2802`); it replaces I5a's tier-0 refusal. P2-CELL is set only by the JIT's pass 2 (`9349`).

Lead note (2026-10-01, from I5a): nothing writes TIER-CELL at x86 boot (`boot-x64.f` has no tier store and DATA maps zeroed), so it reads 0 and the Habu head refuses with the tier-0 message on x86 until something stores 1; x86 `set-tier` stores only 1 (`kernel-x64.f` SET-TIER-BODY), and `executable-build-enter` also sets it. The x86 boot must select tier 1 when this dot makes x86 refuse tier 0.

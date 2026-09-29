---
title: Fail closed on x86 in habu1.f two-arm forms
status: active
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.757499+03:00"
---

Problem: 25 `HB-TARGET-LINUX?` two-arm forms in `src/habu/habu1.f` (cross-build obligation (4)).
Acceptance: each gets an explicit refusal for linux-x86-64 (the x86 engine never runs these emitters); the `docs/porting.md` fail-closed rule holds; the Gforth chain stays ARM64.
Files: `src/habu/habu1.f`.
Verify: spark: rebuild; chain gen2==gen3 (the ARM64 engine is unchanged); gate.
Depends: none. Serialise on `habu1.f`/`habu2.f` with K4, I6 and I10c.
Route: Alder (shared: src/habu/habu1.f).
Ownership: krait (Intel lane).
Claim: agent=krait workspace=.jj-ws/habu-fail-closed-on-f84f1197 (stacked on intel/habu-share-the-primitive-58c235e5).
Preflight corrections (these add to the Acceptance above):
- The 25 forms sit in 24 words, all emit-only `( -- )`: 10 exit forms (`habu1.f` 858, 910, 1006, 1282, 1309, 1334, 1354, 2270, 2315, 2333), 5 inline `IF/ELSE/THEN` (522, 2193, 2200, 2211, 2218), 10 one-arm Linux prologues with a Darwin-shaped shared tail (933, 965, 1864, 1887, 2165, 2172, 2179, 2186, 2290, 2297). Every one is "Linux, otherwise Darwin", the shape `docs/porting.md:65-67` forbids.
- The refusal is one word owned by `habu1.f`: `ENGINE-EMIT:TARGET-UNKNOWN ( -- )`, `s" hb: habu1: no primitive body for this target" 76 die`, defined by reopening `package ENGINE-EMIT` before `GUARD-IOCTL`; its name must not end in `:CALL` (`tools/lint/clobber-lint.f:211-214`). It cannot reuse `habu2.f`'s `C-TARGET-UNKNOWN` (habu2.f loads later; a duplicate top-level name dies at `habu2.f:3457`). Each form names the Linux-aarch64 and macOS arms explicitly and calls it otherwise, so `rg -n 'HB-TARGET-LINUX\?' src/habu/habu1.f` shows no arm left implicit.
- Verify: the product built from this tree by the base engine is byte-identical to the base engine (`11c00585…`); the chain; gate; the Gforth check `HABU_BOOTSTRAP_CHECK_ONLY=1 tools/bootstrap.sh` on spark (the stage engine interprets `habu1.f`, `tools/bootstrap.sh:123,304-322`).

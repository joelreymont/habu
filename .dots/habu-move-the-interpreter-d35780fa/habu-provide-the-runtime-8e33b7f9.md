---
title: Provide the runtime create from Habu
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T12:34:13.020113+03:00"
blocks:
  - habu-compile-definer-bodies-1292d049
  - habu-move-evaluate-and-9119f746
---

Problem: the runtime `create` row (`prims.f` ~797) runs the assembly `LCREATE` on ARM64 (`habu1.f` ~1488 via CREATEP-CELL) and is a REFUSE row on x86 (`kernel-x64.f` ~1712), so load-time `does>` children (`5 CONST FIVE`) get no x86 routine and the stripped image's `STRIP-VALUE` (`test/stripped-image-subject.f:23-24`) cannot link on x86. Split from habu-compile-definer-bodies-1292d049 (I7) by the I7 design (lead correction on I7).
Design (I7 decision 5): after I9a lands `FL-PREFIX-PROVIDED`/`KEEP-BODY?`, a global Habu `create` in `definers.f` that calls `NCOMP:COMPILE-FIXED` is the prefix-provided body; the x86 REFUSE row drops in seeded builds. No second assembly path from LCREATE into NCOMP.
Acceptance: a load-time `does>` child gets ARM64 and x86 routines through the Habu `create`; the x86 linker links a window holding one; the stripped image's `STRIP-VALUE` links on x86; ARM64 chain fixpoint and stripped rows green.
Files: `src/habu/definers.f`, `src/habu/prims.f` (marker), `src/habu/kernel-x64.f`.
Verify: spark rebuild, chain gen2==gen3, stripped family, gate; ThinkPad x86 link image of a `does>` child.
Depends: habu-compile-definer-bodies-1292d049 (I7b), habu-move-evaluate-and-9119f746 (I9a).
Route: Alder (shared: src/habu/definers.f, src/habu/prims.f).
Ownership: krait (Intel lane).
Claim: unassigned.

Lead note (2026-10-02, from I7b): this leaf also owns the shadow half of `does>` (I7 decision 3): a build-time child's x86 routine needs a TAIL site at `emission size - 6` naming the clause. Prefer deriving it at shadow capture from the ARM64 body's patched slot (the slot's branch target is the clause; an unpatched RET slot means no site; a second `does>` is the last write), which needs no hook in `does>` parents. Do not have the elaborator resolve a Habu word by name from every `does>` parent: the manifest loads `does>` definers before `src/compiler/native/shadow.f`.

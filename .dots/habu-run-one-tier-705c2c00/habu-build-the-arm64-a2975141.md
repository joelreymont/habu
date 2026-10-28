---
title: Build the arm64 engine on T0
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T14:52:51.635869+02:00"
blocks:
  - habu-cross-compile-through-66516afb
---

Problem: the arm64 engine still compiles with the JIT. Acceptance: fixed-body sites call FIXED-SHADOW-CELL with (value kind) = T0:COMPILE-FIXED (habu2.f:4949-4957, 5378, 5603, 5785; does> 3944), XT-CELL always T0:COMPILE, prefix rows (t0-emit.f, t0.f, host emitter in EMIT-HOST-LOAD-PREFIX, native-runtime.f manifest, build-fixpoint.f, xcomp.md mirror list), fixpoint gen2->gen3. Commit carries umbrella section 5 measurements against the JIT: load <= 1.2x instructions, run <= 1.05x on zz and tender, identical outputs. A miss is reported with numbers. Leaf-entry reference: the dropped JIT nop-skip measured zz 12.75G -> 12.62G and tender 277.7G -> 268.8G run instructions (bench g2 builds, 2026-10-28); T0-arm64 leaves without an entry save should show at least that gain over the JIT. Files: src/habu/habu2.f, native-runtime.f, tools/build-fixpoint.f, docs/xcomp.md. Verify: full arm64 suite, two-generation build, bench. Depends: L6. Ownership: those files. Claim: unassigned.

From the L5b1 review: src/habu/t0-arm64.f SIGNED?, B-BITS and ADR-BITS copy src/compiler/native/a64ir.f FITS?/B-BITS/ADR-BITS, which go with tier 1. Branch reach belongs in A64ASM (src/arch/arm64/asm.f ENC-B, ENC-BL, ENC-ADR mask without checking); move it there in this lane, which rebuilds the engine anyway, and delete the T0-A64 copy.

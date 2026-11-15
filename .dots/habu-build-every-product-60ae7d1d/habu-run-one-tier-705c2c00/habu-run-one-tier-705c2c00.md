---
title: Run one tier 0 on every target
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T14:52:18.214120+02:00"
---

Joel 2026-10-27/28: iteration speed; one T0 front end (src/habu/t0.f), one neutral emission record, one publisher and capture/shadow path, per-arch emitters only (arm64, x86-64, Wasm); any host cross-compiles any target; arm64 JIT replaced by T0-arm64 and deleted; tier 1 and the Gforth seed retired. Lanes are children.

Claim: jerry, 2026-10-28.

State: the front end is src/habu/t0.f; its emitter seam is src/habu/t0-emit.f (package COMP, EMIT- operations, EMIT-SELECT refusing an architecture with no emitter by E-T0-TARGET); the emitters are src/habu/t0-arm64.f, t0-x64.f and t0-wasm.f (Wasm with structured control: one block per open structure, `begin` a `loop`, a DO level a `block` around a `loop`); the emission record is src/compiler/native/emission.f (ADDR-DATA 1, ADDR-CODE 2); publication is src/compiler/native/publish.f and the shadow src/compiler/native/shadow.f. arm64 compiles and builds through T0; the JIT and tier 1 are deleted; the Gforth seed is retired: recovery is a trusted hb run through tools/seed-main.f, which builds bin/hb with tools/hb-build.f (docs/bootstrap.md "Recovery"). x86-64 is off.

Remaining, each its own child: Wasm modules built through T0 and the one linker (habu-emit-wasm-from-0d5dab47); what tier 1's deletion left (habu-delete-tier-1-f5c44fb4); a callback's C entry made in the step that defines it (habu-make-a-cb-6de47af1); T0's control joins, tail calls and performance (the other open children).

Order, Joel 2026-10-31, after arm64: "after you are done with trusted you should start looking at x86 and wasm."

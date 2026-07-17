---
title: "Mirror: emit external prim names over 16 chars"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-17T01:53:09.050845+02:00\""
---

Capability gap found by the pretrust lane's mirror-bug RCA (2026-07-17): the gforth recovery mirror (bootstrap/cg/forth.fs EMIT-EXE -> BUILD-MACHO path) only emits INLINE prim names up to DNAME-INL (16 bytes); a longer FPRIM name needs external DNAME-EXT storage the mirror does not emit, so the fixup walk chases a garbage name pointer and wedges gforth in an EXC_BAD_ACCESS signal/longjmp loop (~40 min, no progress, address=-16) during BUILD-MACHO. Native handles long prim names fine (has 21-char prims). The pretrust lane hit this with DRAIN-PRE-TRUST-DEFERS (22 chars) and resolved by renaming to DRAIN-PRETRUST (14) - the convention-respecting fix - so nothing is broken today; this dot is the standing capability so a >16-char prim name is expressible if it ever becomes unavoidable, OR a fail-closed guard so the mirror dies with a named diagnostic instead of wedging. Work options: (a) emit DNAME-EXT external name records in the mirror's dictionary emission (full parity with native), or (b) minimum: a build-time length check in the mirror's FPRIM registration that dies 'stage0: prim name exceeds inline cap: <name>' - turning a 40-minute silent wedge into an instant diagnostic (this alone is worth landing even if (a) follows). RCA method worth keeping (already in LESSONS via the pretrust lane): sample to prove fault-loop-not-slowness, lldb for the address, clean-master timing baseline, EMIT-FORTH-vs-EMIT-EXE bisect, then name-length bisect. Acceptance for (b): a >16-char FPRIM in a scratch mirror build dies instantly with the named diagnostic; existing prims unaffected; mirror-lint/bootstrap-codegen/recovery green. Files: bootstrap/cg/forth.fs (+ pinned counts if moved). Ownership: bootstrap/stage0 codegen.

Claim: agent=mirext workspace=.jj-ws/fable-mirext (host lane - bootstrap/cg only)

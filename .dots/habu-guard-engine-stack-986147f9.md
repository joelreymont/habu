---
title: Guard engine stack extents at runtime
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:19:53.854959+02:00"
blocks:
  - habu-derive-fixed-data-853cb615
---

Static invariant: every native or recovery data-, user-return-, and loop-stack memory access is preceded by a bounds proof against the authoritative DATA-LAYOUT extent. Problem: J-TOR, J-RPOP, EMIT-P2-RS, J-FRAME, matching pops, and loop-frame paths can cross fixed capacities without a named guard, corrupting adjacent DATA cells before a diagnostic. Fix: derive stack bases/limits/capacities from DATA-LAYOUT, add fail-closed push/pop/frame guards at the shared native seams, and mirror the exact policy in bootstrap. Acceptance: zero-depth pops, capacity+1 pushes, 2>r/2r> boundary transfers, nested loop frames, address wrap, and mismatched native/recovery capacity reject before access with named errors; exact-last-slot operations pass; adjacent sentinels never change. Files: src/habu/layout.f, src/habu/habu2.f, bootstrap/cg/forth.fs, new test/engine-stack-bounds.f. Verify: focused mutation matrix, engine suite, bootstrap parity, clobber lint, typed-local diff, fixpoint, host/dot lints, full native gate. Dispatch only after DATA-layout and overlapping engine/bootstrap owners clear.

Revalidated audit M1/M2 on 2026-09-14 at source 220930e479bdff0f21819944ea6aefbfe380e798 and optimized native engine SHA-256 2a49a29c9804f00292652d45f0a32aa27e6f224034585501e890bf9d2acbc14c. EM-RUNTIME-STACK (habu2.f:4558) reserves 16384 bytes below process SP; G-PUSH (rt.f:29) stores then increments without an upper bound, and the LMAIN guard (habu2.f:6527) checks only the floor. A read-only live geometry probe confirms argv minus S0 is 16392 bytes: 2048 cells plus argc. RSTK-PUSH/POP (habu1.f:1562) remain unguarded, with RSTK-OFF $2800 plus 256 cells ending at LOCNAMES $3000. Native disassembly of public 2>r confirms unchecked stores/depth increments; the safe 1 2 2>r 2r> pair returns 1 2.

The original compiled >r reproducer is not evidence for tier 1: src/compiler/native/elaborate.f models ordinary return transfers as compile-time value vectors with checked vector capacity, without accessing the engine RSTK region. Fix the still-live primitive and actual stack-access paths; do not restore old host/compiler machinery to reproduce the old audit. No overflowing data/return-stack operation was executed during this revalidation. The existing runtime guard task owns both findings; peak-certificate work remains a separate child of the stack coordinator.

2026-09-14 independent Astra design review against6d60ebba: a G-PUSH/G-POP-only
patch is incomplete. Native emit.f has direct DSTACK loads/stores and pointer
adjustments; JIT/pass2 templates have direct traffic. BRUNSTACK ignores its size,
task entry installs another allocation, and catch/evaluate can restore x19 across
those switches. Bounds must belong to the active allocation and follow all of
those transitions, including snapshot/stripped boot. layout.f is the current
authority; no DATA-LAYOUT package exists. Runtime loop storage is32 frames in
[LOOP-STK-OFF,BODYBUF-OFF), separate from native compiler loop IR.

Design: shared access-span, pointer-move and indexed-depth guards; validate
alignment/order and use unsigned/subtractive comparisons before address math.
Preflight both sides of each whole multi-cell transfer before any mutation.
Preserve GPR/FP/NZCV/LR and net machine SP on success; no stack-based reporting
on the failure path. Refusals must be relocation-safe in stripped output, not
conditional branches to an engine-only label. Update native OP-INSNS/source-map
sizing with emitted guards. A future peak proof may eliminate redundant checks;
it must not substitute an unproved declaration for a physical bound.

Ownership split after guard ABI review: one owner for engine/layout/lifecycle
(rt.f, habu1/2.f, jit.f, aot-lib.f, snap-lib.f, lib/task.f); one for native emit.f;
one for recovery parity/fixtures. Bootstrap G-PUSH/POP are in cg/templ.fs and
serve another standalone arrangement, so audit consumers before mirroring.
Acceptance includes last-slot success/next-slot refusal, all zero-depth reads,
atomic2-cell transfers, loop32/33 and j/one-frame, malformed/wrapped descriptors,
run-in-stack/task/caught-cross-stack-throw, native/JIT/interpreted/stripped and
recovery paths with unchanged adjacent sentinels. No invalid access was executed
during this design review. Implementation remains open.

2026-09-14 checkpoint: reviewed allocation ABI 2f3e0f90 is integrated dcd369bf;
run-in-stack now owns test helper switching too (a0b2ec7a). Native emitter guards
and safe verifier fixtures are in .jj-ws/cedar-native-stack. The old 10 MiB code
region exhausted at EM-COMPILE-UNDEF with E-NPUB-ROOM (-8561). Joel explicitly
chose a fixed capacity budget: 32 MiB region, 30,404,608 code bytes after dictionary,
no allocator growth. Integrated 90dc1891 includes the recovery and relocation
model mirrors; its transitional engine rebuild, reloc-proof and snapshot-writer
pass. Guarded rebuild/code-usage measurement is still running. Engine/JIT slice
ffb57fb5 has focused passing evidence and is under independent review; wide
transfers, remaining primitives and recovery parity are still open. This dot
does not claim complete physical stack coverage or a green combined gate.

2026-09-14 integration checkpoint: native guards 6739c962, emission fixture
f8104159 and complete primitive/wide JIT transfers 61b07c76 are independently
reviewed and integrated. Guarded generation 2 succeeds with 16,076,868 region
bytes and 124,137 call rows. Capture budgets 7a16d3de use the bounded code-band
blob and 163,840 rows. No unbounded growth was introduced.

Native debugger cursor correction 16776919 and Hazel's reviewed wide-memory,
loop 32/33 and tty REPL recovery fixtures are integrated. Combined r1, source
93a972eb and SHA 07d6f0db, builds and passes source-free startup plus focused
wide/lifecycle/JIT/debugger/stack-switch/PTY checks. Its same-source r2 rebuild
is running; Hazel owns the subsequent complete native gate.

Recovery checkpoint a2b52038 closes the earlier drop/2drop/nip and return/loop
request gaps. Actual recovery wide transfers, guard ABI/register preservation,
physical boundaries and direct/evaluate unwinds pass. Independent production
review and the final test/launcher commit remain pending. The debugger test
exposed recovery C-CALL copying a patched BRK into a caller without a matching
breakpoint-table entry; the engine owner is correcting inline eligibility.
Hazel independently validates the final recovery chain. This dot stays open
until recovery and combined qualification pass.

---
title: Decide whether one pass can replace tier 1
status: active
priority: 1
issue-type: task
created-at: "2026-10-23T18:58:18.936516+03:00"
---

Joel (2026-10-23): can ARM64 tier 0 emit code as fast as tier 1 without the IR, register allocator and optimizing passes? The optimizing layer was added on a model's claim that an IR is the way to fast code; no benchmark backed it. Evidence so far (2026-10-23, engine at intel dfa5151b, ~/.cache/tmp/jerry-tiers/): tier 0 compiles a trivial definition in 36 us, tier 1 in 640 us; tier-1 code ran 1.45x (FIB), 9.7x (SUMSQ) and 3.6x (MIX) faster. In those listings every tier-1 gain is single-pass-capable: primitives inlined (tier 0 calls them), stack values in registers, ?do index and limit in registers (tier 0 reloads them from a memory loop stack, 5 instructions per i), compare fused with an immediate; tier 1 hoists nothing and leaves redundant movs. Not single-pass: the counted-loop fold (loop.f), whole-word residency and register allocation. Until this is settled ARM64 keeps both tiers (docs/architecture.md). Acceptance: measured answers on the real corpora (habu, maki, zen, tender, etch, dots, zzusage, kiba, blackboard, loom) from the children below, and Joel's ruling recorded in docs/architecture.md. Claim: jerry.

Evidence for the ruling (2026-10-23, children census aed47e19, timing d02bf855, ablation a8a15eed, classification df654e77):
- Compile: tier 1 loads 11-23x slower; whole-program time (load plus run) is lower at tier 0 on zz, dots and tender, the three real workloads timed.
- Run: tier 1 is 1.25-3.4x faster (zz 3.4x, tier-bench 2.5x, tender 1.65x, dots 1.3x; dots is 70% syscalls at both tiers).
- Where the speed comes from: on zz and tender hot words, 94.6% and 98.0% of the sample drop is primitive inlining, register-resident stack values in straight-line code, loop counters in registers, compare/branch fusion and tail calls, all single-pass-capable; residency across joins plus register allocation and spills is 5.4% and 2.0%.
- Passes a single pass cannot do: the loop fold fires 0 times on every corpus; disabling copy coalescing or cross-join residency knowledge changes no real workload's run time beyond noise.
- Not measured: etch and loom (do not load on current Habu: habu-make-etch-load-ba7e4997, habu-make-loom-load-55f9444c); maki run time (its tests run geometry in a separate OCCT process).
The next step on this evidence is the proposed experiment habu-inline-primitives-in-74e0211f, which waits for Joel's go-ahead.

Joel's rulings (2026-10-25 interview):
- Run experiment habu-inline-primitives-in-74e0211f before ruling; it is approved.
- End state if one pass wins: single-pass codegen on every platform (ARM64, x86-64, Wasm), then tier 1 and the IR are deleted.
- One pass wins when real workloads run within 5% of tier 1's run time and compile cost stays near today's tier 0.
- The ruling waits for every corpus: etch and loom are made to load on current Habu (habu-make-etch-load-ba7e4997, habu-make-loom-load-55f9444c) and timed with the rest.
- Until then every confirmed tier 1 defect is dotted. One a real program hits (x86-64 and Wasm run only tier 1) gets its root fix; the rest wait for the ruling, which deletes them with tier 1 or schedules them. No new tier 1 optimization work.
- Workloads timed for the ruling, each made to run on current Habu first: zz, dots, tools/tier-bench.f, tender, zen (formerly Kaizen) under a real query load, maki's CAD server building real designs end to end (Habu time split from the OCCT worker's; maki tests.f exits 67 from its kernel worker today), etch and loom.

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
- Not measured: etch and loom (made to load: etch master e182e7a9, habu-make-etch-load-ba7e4997 closed; loom aspen/dots-import c5bbb76d, habu-make-loom-load-55f9444c closed); maki run time (its tests run geometry in a separate OCCT process).
The next step on this evidence is the proposed experiment habu-inline-primitives-in-74e0211f, which waits for Joel's go-ahead.

Joel's rulings (2026-10-25 interview):
- Run experiment habu-inline-primitives-in-74e0211f before ruling; it is approved.
- End state if one pass wins: single-pass codegen on every platform (ARM64, x86-64, Wasm), then tier 1 and the IR are deleted.
- One pass wins when real workloads run within 5% of tier 1's run time and compile cost stays near today's tier 0.
- The ruling waits for every corpus: etch and loom now load on current Habu (habu-make-etch-load-ba7e4997, habu-make-loom-load-55f9444c, both closed) and are timed with the rest.
- Until then every confirmed tier 1 defect is dotted. One a real program hits (x86-64 and Wasm run only tier 1) gets its root fix; the rest wait for the ruling, which deletes them with tier 1 or schedules them. No new tier 1 optimization work.
- Workloads timed for the ruling, each made to run on current Habu first: zz, dots, tools/tier-bench.f, tender, zen (formerly Kaizen) under a real query load, maki's CAD server building real designs end to end (Habu time split from the OCCT worker's; maki tests.f exits 67 from its kernel worker today), etch and loom.

Experiment result (habu-inline-primitives-in-74e0211f, all steps landed and measured, 2026-10-27): tier 0 now inlines primitives, keeps stack values in registers across joins (pools 15 int, 24 float), fuses compare+branch and caches the innermost loop index. Tier 0 run as a multiple of tier 1 on current master: zz 1.34x, tender c2-office-scale 1.18x, dots ls 1.14x, dots tree 1.10x, tier-bench 1.04x whole (kernels 2.80x); compile cost unchanged (trivial definition 36 us vs ~585 us at tier 1). The 5% bar is not met on any real workload. In the dumped hot words the remaining cost is calls inside loop bodies (spills, reloads, index store-back). The planned steps are exhausted. Still untimed for the ruling: zen under query load, maki, etch, loom. Ruling: Joel's.

Joel's ruling (2026-10-27 session): "iteration speed is what i think matters, ultimately. i think we should be using tier 0 for everything and MAYBE keep tier 1 around if it's twice as fast." Measured with tier 0 after the speed stack (master 5f2f6488): tier 1 runs zz 1.17x and tender 1.24x fewer instructions (zz user time equal), maki AOT product 0.6-0.8%; tier 0 compiles 3-27x cheaper (zz load 1.78G vs 42.9G insn, AOT builds zen 2.9x, maki 4.5x). Tier 1 is not twice as fast anywhere, so it goes. This replaces the 5% bar. Chosen route (interview): port tier 0 to x86-64 and Wasm (habu-give-x86-64-400065cc), then delete tier 1, salvaging what is worth carrying into tier 0 first: locals in registers (habu-copy-on-write-a885df73) and the virtual stack across do loops are scheduled. Census (master f3d192cd): tier 1 only ~74.5k lines, tier 0 ~6.2k; blockers are x86-64/Wasm (no tier 0), the engine build forcing tier 1 for its target (tools/native-build-core.f AT-TIER-1), shared files inside src/compiler (native/string.f literal interner, workspace.f, native/dict.f), file/unit object cache and FFI CALLBACK: (tier 1 only). Remaining for this dot: record the ruling in docs/architecture.md.

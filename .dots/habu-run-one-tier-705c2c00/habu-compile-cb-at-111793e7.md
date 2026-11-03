---
title: "Compile CALLBACK: at the caller's tier"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-03T10:15:11.511573+02:00"
closed-at: "2026-11-03T12:14:26.240382+02:00"
close-reason: landed as master 811162e8 (cbx86)
---

Problem: src/lib/ffi-callback.f GEN-NATIVE (:306-311) forces `1 set-tier` around every CALLBACK: accessor ("The C entry needs a tier-1 emission", written when tier 0 was the JIT), so T0's C entry stub (src/habu/t0-arm64.f:439-446, t0-x64.f:181-186) never runs, and on x86-64 the forced tier 1 has no stub (emit-x64.f), so accessors fail -8570 (E-NCOMP-STATE). test/ffi-callback-growth.f:2, ffi-callback-full-image-prefix.f:2 and ffi-callback-full-image-rebind.f:2 select tier 1 for nothing tier-1-specific. Found by the callback-stub critic (rev 1 REJECT of habu-give-the-cb-e6876f60). Acceptance: GEN-NATIVE (private) and its comment go, its caller calls GEN-RUN; the three `1 set-tier` lines go; tier 1's stub stays covered by test/engine-stack-machine.f. No public item changes. Files: src/lib/ffi-callback.f and the three tests. Verify: arm64 rows ffi-cabi, ffi-callback, ffi-callback-growth, ffi-callback-zero-image, ffi-callback-full-image, ffi-callback-file-cache, engine-stack-machine, native-file-state; the same rows on pulsar with an x86-64 cross-built engine, before and after. Open, measured by the lane: whether a T0-compiled accessor survives capture, image relocation and the file cache on both machines. Brief /tmp/jerry-big/cbtier-brief.md. Depends: none. Ownership: those four files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-cbtier.

Landed 2026-11-03 as master cd2368be. arm64: the 8 rows rc 0 at the rebased tip with the installed engine (master 3f03ec9e's), /tmp/jerry-cbtier-tmp/tip/*.out; x86-64 cross-build rc 0 (jerry-cbtier/.out/x86/hb). The acceptance line "tier 1's stub stays covered by test/engine-stack-machine.f" is false: engine-stack-machine's callback case now runs T0; tier-1 stub emission stays covered by native-file-state HOOKS-CASE, and nothing calls a tier-1 stub from C. Not restored: no tier-1-only test work (LESSONS "editing tier 1"). Open: the x86-64 rows on pulsar, before (/tmp/jerry-cbtier-tmp/base) and after (the landed tree), engine jerry-cbtier/.out/x86/hb, recipe /tmp/jerry-big/cbtier-brief.md proof 2; pulsar-ua-106 port 22 timed out. Stays active until they run.

x86-64 rows, 2026-11-03 on joel@pulsar-ua-106, one engine (jerry-cbtier/.out/x86/hb, cross-built from cd2368be) over the tree before (cbtier.diff reversed, pulsar /tmp/jerry-cbtier-base) and after (/tmp/jerry-cbtier); logs pulsar /tmp/jerry-cbtier-base-out/*.out and /tmp/jerry-cbtier-out/*.out. Row: before → after.
- ffi-cabi: rc 83 → rc 83, "whitebox-engine: unsealed engine build failed" both sides (not cbtier's).
- ffi-callback: rc 67 -8570 → rc 139, segfault (crash dump rip 0x40f46e, rax 0).
- ffi-callback-growth: rc 67 -8570 → rc 106 "hb: callback: slot is not bound" (kernel-x64.f :4739/:4795, the row's binding cell is 0).
- engine-stack-machine: case "a thread C started, inside a callback" rc 67 -8570 → signal 11.
- ffi-callback-full-image: rc 67 -8570 → rc 0.
- ffi-callback-zero-image, ffi-callback-file-cache: rc 76 "source-exec-run is not in the x86-64 kernel" both sides (habu-give-the-x86-4f58a6ff).
- native-file-state: rc 67 -2101 both sides.
So on x86-64 accessors now reach T0's stub, and a call through it crashes or finds its slot unbound: not proven. Root cause and fix: lane cbx86, workspace .jj-ws.noindex/jerry-cbx86, brief /tmp/jerry-big/cbx86-brief.md. Stays active until it lands.

Closed 2026-11-03: lane cbx86 landed as master 811162e8 (src/habu/kernel-x64.f only): CB-THUNK's row index scales by CB-ROW-BYTES, the dispatch index by the 5-cell descriptor, and a C-started thread gets an alternate signal stack. x86-64 engine cross-built from 811162e8 on pulsar: ffi-callback-growth rc 0, ffi-callback-full-image rc 0; reduced reproducer pulsar /tmp/jerry-cbx86-scratch/r3.f (qsort through slot 1) rc 106 "slot is not bound" before, "sorted 1 2 3 4" rc 0 after. ffi-callback rc 83 and engine-stack-machine rc 1 stop at the whitebox engine build's hb-build exit 83, the x86-64 package-part defect (habu-open-a-pkg-208310aa); ffi-callback-zero-image and ffi-callback-file-cache stay at habu-give-the-x86-4f58a6ff.

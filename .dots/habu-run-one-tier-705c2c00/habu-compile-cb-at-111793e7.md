---
title: "Compile CALLBACK: at the caller's tier"
status: active
priority: 2
issue-type: task
created-at: "2026-11-03T10:15:11.511573+02:00"
---

Problem: src/lib/ffi-callback.f GEN-NATIVE (:306-311) forces `1 set-tier` around every CALLBACK: accessor ("The C entry needs a tier-1 emission", written when tier 0 was the JIT), so T0's C entry stub (src/habu/t0-arm64.f:439-446, t0-x64.f:181-186) never runs, and on x86-64 the forced tier 1 has no stub (emit-x64.f), so accessors fail -8570 (E-NCOMP-STATE). test/ffi-callback-growth.f:2, ffi-callback-full-image-prefix.f:2 and ffi-callback-full-image-rebind.f:2 select tier 1 for nothing tier-1-specific. Found by the callback-stub critic (rev 1 REJECT of habu-give-the-cb-e6876f60). Acceptance: GEN-NATIVE (private) and its comment go, its caller calls GEN-RUN; the three `1 set-tier` lines go; tier 1's stub stays covered by test/engine-stack-machine.f. No public item changes. Files: src/lib/ffi-callback.f and the three tests. Verify: arm64 rows ffi-cabi, ffi-callback, ffi-callback-growth, ffi-callback-zero-image, ffi-callback-full-image, ffi-callback-file-cache, engine-stack-machine, native-file-state; the same rows on pulsar with an x86-64 cross-built engine, before and after. Open, measured by the lane: whether a T0-compiled accessor survives capture, image relocation and the file cache on both machines. Brief /tmp/jerry-big/cbtier-brief.md. Depends: none. Ownership: those four files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-cbtier.

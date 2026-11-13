---
title: "Hash a build's inputs with the CPU's SHA-256 instructions"
status: closed
priority: 3
issue-type: task
created-at: "2026-11-07T09:08:12.458945+02:00"
closed-at: "2026-11-10T21:50:20.557685+02:00"
close-reason: "done: landed as 1e746920 \"Add the sha256-blocks primitive\" and 30ae78b6 \"Hash blocks with the CPU's SHA-256 instructions\"; builds no longer hash"
---

Problem: every warm build hashes hb's own file for ENGINE-ID:KEY$ (src/lib/engine-id.f), every source it loads, and every object it links (object self-check, src/habu/linker.f HEADER-CK); a build that writes hb signs it, hashing hb again. src/core/sha256.f runs at 71 MB/s on the T0 hb (/tmp/carl-a2wb/hb, 8.1 MB in 114 ms) and 89 MB/s on the installed tier-1 hb (7.2 MB in 81 ms). On the census of hb's startup rows 0-14 (carl-a2w, `zsh .jj-ws.noindex/carl-dots/.out/kept/c5a2/a2w/run.sh`, profile census-p.f) SHA-256 is 234 of the 517 ms sampled in the warm run: ENGINE-ID 120, object checksums and source sums 93; checker.f's object alone is 3.7 MB.

Lead's pick (carl, 2026-11-07): the engine key stays the SHA-256 of hb's bytes. A build ID written into hb would save the ~4 ms a hardware hash of 8 MB costs, but adds a saved format, makes the key the writer's claim rather than the bytes', and needs a second path until the cross-build linker writes hb; macOS's code-signature hash has no Linux twin. Hashing fast removes the ENGINE-ID floor and the source and object hashing together.

Plan: a primitive that runs SHA-256's block function with the CPU's instructions (arm64 SHA256H/SHA256H2/SHA256SU0/SHA256SU1; x86-64 SHA-NI where present), src/core/sha256.f's Habu code staying the body where a machine has none. It is a new engine primitive: its design goes to Joel through the interview skill as soon as it is written. Every backend gives a primitive a body (src/habu/prims.f, primitive-registry.f COMPLETE), so the x86-64 body waits on "one arch first" (no x86 design items until arm64-to-arm64 builds work). The arm64 assembler has no SIMD or SHA instructions yet (src/arch/arm64/asm.f, mnem.f).

Acceptance: KEY$ and every digest unchanged byte for byte against the Habu code (FIPS-180 vectors plus hb's own file); the census profile's SHA-256 share below 5%. Verify: src/core sha256 test vectors on both bodies; the census pair and its profile. Depends: arm64-to-arm64 builds for the x86-64 body. Ownership: src/core/sha256.f, prims.f row, arm64 assembler. Claim: unassigned.

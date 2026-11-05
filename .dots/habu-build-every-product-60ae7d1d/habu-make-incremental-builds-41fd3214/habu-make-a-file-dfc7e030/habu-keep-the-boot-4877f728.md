---
title: "Keep the boot out of a booted product's kernel digest"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T01:09:26.518389+02:00"
---

Problem: FILL-KERNEL-DIGEST (src/habu/habu2.f:13900) hashes code bytes 0 to KERNEL-CODE-END, so once a start-up sits at offset 0 before the kernel (src/arch/arm64/boot.f, LINK-STRIPPED in src/habu/link-data.f) the digest covers the boot too, with its four tail-label offsets still unpatched when the hash runs. BKERNELPRODUCER's comment 'from the final immutable kernel bytes' is then false for such a product. Today harmless: its one reader, src/core/source-producer.f KERNEL$, finds the primitive through a dictionary a stripped product lacks. It matters once hb itself starts through ARM64:OPEN, the boot, ARM64:KERNEL (design-s6-l5.md L5-1). Acceptance: the digest hashes the kernel's bytes only (from the kernel's first byte), or the boot's bytes are final before it, with BKERNELPRODUCER's comment true; hb built before and after is byte-identical while hb has no boot. Files: src/habu/habu2.f. Verify: bin/hb --load test/run.f -- arm64-place, and the hb build's byte-identity check. Depends: the step that boots hb through A64BOOT. Ownership: unassigned. Claim: unassigned.

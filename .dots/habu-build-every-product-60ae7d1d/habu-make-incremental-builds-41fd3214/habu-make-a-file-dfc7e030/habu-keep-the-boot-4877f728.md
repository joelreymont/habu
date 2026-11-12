---
title: "Keep the boot out of a booted product's kernel digest"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T01:09:26.518389+02:00"
---

Problem: FILL-KERNEL-DIGEST (src/habu/habu2.f:3345-3350) hashes code bytes 0 to KERNEL-CODE-END. src/arch/arm64/kernel-object.f EMIT (:73-87) writes the start-up (src/arch/arm64/boot.f, ENGINE-START, for an engine) at code offset 0 before the kernel and leaves its tail labels unbound for the link, then runs ENGINE-EMIT:KERNEL, so the digest covers the boot with those offsets still unpatched. hb's own build takes this path (tools/hb-build-core.f:275-278 KERNEL-BODY, :295-299 KERNEL-BUILD), so BKERNELPRODUCER's comment (habu2.f:3323-3325) 'from the kernel bytes as emitted' does not describe what is hashed. The digest's reader is reached on every load of the native compiler: src/compiler/native/host.f:568 NHOST:INSTALL, KERNEL-OWNER (:215-225), src/core/source-producer.f KERNEL$ (:23-28), the kernel-producer$ primitive (habu2.f:3388). KERNEL-OWNER uses only the digest slot's address, as the end of the kernel's code interval, and the text base as its start, so that interval also takes in the boot; nothing reads the 32 hash bytes.
Acceptance: the digest hashes only the kernel's bytes, from the kernel's first byte (kernel-object.f K0) to KERNEL-CODE-END, so a change to the boot alone leaves it unchanged, and BKERNELPRODUCER's comment and KERNEL-OWNER's kernel interval say and use the kernel's extent.
Depends: none.
Claim: unassigned.

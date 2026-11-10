---
title: "Cross-OS 2: hand the kernel object to the build tool to write"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T10:08:42.743042+02:00"
closed-at: "2026-11-10T10:29:09.121317+02:00"
close-reason: "landed; engine and kernel object byte-equal, --target linux-aarch64 --engine writes its kernel object and stops rc 74 at the link, full suite 270 of 270; the review's two edits are the commit on top"
---

Step 2 of the cross-OS design (.jj-ws.noindex/carl-layout/.out/design-cross-os-build.md, item 4). Problem: a kernel build runs the target-compiled KERNEL-OBJECT:WRITE here and it writes its own file through FS:ATOMIC-WRITE-FILE (src/arch/arm64/kernel-object.f:237-238), a C call that cannot run when the target is another OS: --target linux-aarch64 --engine crashes rc 139 at a null foreign call before any object is written (measured 2026-11-10, carl-xos/.out/x2). Acceptance: WRITE ( bool [ ptr u8 n -- ] -- ) hands the object's bytes to a sink; tools/hb-build-core.f names and writes the file; kernel-object.f requires no fs file; a build without --target is byte-equal, kernel object included; the Linux build writes its kernel object and stops at the link's layout refusal. Files: src/arch/arm64/kernel-object.f, tools/hb-build-core.f. Verify: brief .jj-ws.noindex/carl-layout/.out/briefs/xos-2-writer-sink.md. Depends: habu-cross-os-1-f4c2b423. Ownership: those files. Claim: agent=carl workspace=.jj-ws.noindex/carl-xos2

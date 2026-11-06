---
title: Measure how much of the kernel a stripped program reaches
status: closed
priority: 2
issue-type: task
created-at: "2026-11-06T02:23:37.595587+02:00"
closed-at: "2026-11-06T02:54:31.475411+02:00"
close-reason: "census done (carl-reach @ 8f7b5f77, report .jj-ws.noindex/carl-dots/.out/carl-file/reach-report.md, probe reach-probe/): LR-HI reaches 2,068 and stripped-pool-reach's app 2,700 of the kernel's 207,552 bytes (1.0%, 1.3%); EMIT-MAIN 90,604, the dictionary sections, EMIT-DICT and the data blocks are unreached; the cut is dot habu-lay-only-the-ac62fed7"
---

Problem: ARM64:KERNEL (src/arch/arm64/link-kernel.f:78 ENGINE-EMIT:KERNEL) lays the whole arm64 kernel, 207,552 bytes, into every stripped program; LR-HI's program reaches a few primitives (census .jj-ws.noindex/carl-dots/.out/carl-file/fill-report.md). Once DATA travels as non-zero runs (habu-write-a-stripped-d7de6482) the kernel is most of a ~270 KB program. Acceptance: for LR-HI and stripped-pool-reach's app, the kernel primitives each program's closure reaches and their bytes against the whole kernel, and what a kernel emission limited to them would need (path:line); a cut becomes its own dot. Files: read only. Verify: the measurement's numbers. Depends: habu-write-a-stripped-d7de6482. Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-reach (census worker, brief .jj-ws.noindex/carl-dots/.out/carl-file/brief-reach-kernel.md).

---
title: Complete shared library reviews and image lifecycle integration
status: open
priority: 2
issue-type: task
created-at: "2026-09-10T18:03:13.386256+03:00"
---

Owner: Cedar for Habu integration; peers retain their isolated product/library changes. Reviewed candidates pending integration: FMATH 954c7335 with lib/errors.f, ELF32 231213d3, Unicode Uppercase 43234129, and FS:SAMEFILE faa83580 (review still pending). UDP/serial factoring candidate679a955a and generic XMODEM are with Kestrel; F64-TEXT60482daa needs lifecycle review/fix before image use. Do not merge stale libc handles, function addresses, locale objects or errno pointers into images. Checked IMAGE-LIFECYCLE REGISTER/PREPARE exists in e9fd7e6d; tests passed reverse order, 2048 growth, reuse, failed cleanup retry and registration during cleanup. Communicate the retry fix to Maki and have task/UDP/serial/F64 owners reset/reacquire and re-register process state through this shared API. Preserve pending Maki RF/per-net-rules read-only review at b44cb251 and peer handoffs already accepted; product policy stays in product repos. Acceptance: reviewed coherent library revisions, matching error registry, focused native API tests and fresh-process cache reinitialization. No new peer feature scope until the core path is settled.


Concrete peer regressions retained after the reassessment: BUF:DISPOSE and VEC-DISPOSE must clear DATA as ownership is consumed, including already-dead headers (Maki 14:56). FMATH:FROUND misrounds 0.49999999999999994 to 1 because x+0.5 rounds early; use fractional-part comparison with symmetric negative handling and preserve range guards. Reproducer: 48828125.0 0.00000001024 f*; verify neighbors of 0.5 and larger half-integers (Maki 15:06). Kestrel XMODEM candidate is 564a2550, with 57 codec and 45 real-PTY checks reported; still pending shared integration.


Completed isolated handoff .jj-ws/cedar-library-handoff parenta0a76c6e (bin SHA256 a5f32af643d53a0e22f2a011aa097b7d8f291dcccf9462ae8003641df33e3eaa): fmath integration+fractional FROUND correction, BUF/VEC cleared DATA/LEN/CAP incl dead headers. Independent Astra review no blockers; native fmath/vector/byte-buffer suites pass. Final integration/push pending. Kestrel found lifecycle REGISTER uses unsynchronized COUNT/HOOKS while UDP/SERIAL initialization can happen in different TASK pthreads; reduce and fix shared registration including task DATA-region semantics. PREPARE remains quiescent capture, no per-app lock workaround.

Kestrel additive target/network library candidate73983518 duplicates the tested564a2550 stack onto a0a76c6e. Matching image must capture its additive error registry: merely overlaying lib/errors.f leaves E-A32ASM-OPERAND/E-UDP4-OPERAND undefined because require considers the old registry provided. IMAGE-LIFECYCLE registration also needs a defined concurrent TASK initialization path; COUNT/HOOKS currently mutate without synchronization. PREPARE is quiescent, but registration can occur on multiple threads. Do not introduce application-local compensations.

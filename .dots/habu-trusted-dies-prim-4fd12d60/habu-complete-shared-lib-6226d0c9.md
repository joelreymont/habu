---
title: Complete shared library reviews and image lifecycle integration
status: open
priority: 2
issue-type: task
created-at: "2026-09-10T18:03:13.386256+03:00"
---

Owner: Cedar for Habu integration; peers retain their isolated product/library changes. Reviewed candidates pending integration: FMATH 954c7335 with lib/errors.f, ELF32 231213d3, Unicode Uppercase 43234129, and FS:SAMEFILE faa83580 (review still pending). UDP/serial factoring candidate679a955a and generic XMODEM are with Kestrel; F64-TEXT60482daa needs lifecycle review/fix before image use. Do not merge stale libc handles, function addresses, locale objects or errno pointers into images. Checked IMAGE-LIFECYCLE REGISTER/PREPARE exists in e9fd7e6d; tests passed reverse order, 2048 growth, reuse, failed cleanup retry and registration during cleanup. Communicate the retry fix to Maki and have task/UDP/serial/F64 owners reset/reacquire and re-register process state through this shared API. Preserve pending Maki RF/per-net-rules read-only review at b44cb251 and peer handoffs already accepted; product policy stays in product repos. Acceptance: reviewed coherent library revisions, matching error registry, focused native API tests and fresh-process cache reinitialization. No new peer feature scope until the core path is settled.

---
title: Widen the AOT capture format past its 64KB world
status: open
priority: 2
issue-type: task
created-at: "2026-08-11T17:49:45.565263+02:00"
---

Prerequisite of every baked-code route (Stage B reframe on habu-seed-the-chain-e98b03d4): the capture format is u16 everywhere - call-site rows (blob-off u16, name-off u16) with an explicit boff $FFFF > die at aot-capture.f:125, DATA-site/CODE-site/window-XT offsets all u16 = a hard 64KB blob ceiling against the chain's 1.15MB. Plus capacities: AOT-BLOB-CAP 64KB, AOT-REC-MAX 256 (chain needs ~6554), AOT-NAMES-CAP 16KB (~51KB needed), AOT-WINDOW:DATA-CAP 64KB, AOT-SITE-MAX 2048, AOT-DSITE-MAX 512, XTOFF-MAX 64; and 45 chain records have names > 16 bytes = EXT-name records, which capture refuses outright (aot-capture.f:172). Widen u16 -> u32 across the four tables AND the boot-side walkers in habu2.f, lift the capacities, accept EXT names. Acceptance (falsifiable): capture and boot a blob larger than 64KB. Unpriced number flagged by the design lane: EM-AOT-PATCH-SITES at chain scale (tens of thousands of LFIND-per-site) - probe by synthesizing a blob with N sites and timing the seed pass against N; a snapshot restore of 15.8MB/5839 records boots in 0.015s as the proxy bound. Files: src/habu/aot-capture.f, src/habu/habu2.f, src/habu/habu1.f (labels). Depends: habu-aot-has-no-0b01043c (widening without the pre-window ruling builds a format for a capture that still dies by name).

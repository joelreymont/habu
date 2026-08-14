---
title: Widen the AOT capture format past its 64KB world
status: closed
priority: 2
issue-type: task
closed-at: "2026-08-14T14:30:41.186213+02:00"
close-reason: "Riders completed at f85ce686 (two commits): the acceptance (capture AND boot >64KB) was landed-but-unasserted on macOS (the PTY forge self-skips green) - now asserted IN BATCH unconditionally, ASLR-proof; the missing CODE-literal producer built (ACAP-OUT-CHAIN carries a pre-window ['] by name via the BL scan's own reverse lookup; red-first at the exact offset). TWO FINDINGS DOTTED: LAOTWIDGATE is O(sites x dictionary) - 85ms/boot at chain scale, paid per boot since arm-seed (the leaf's cost model named the wrong term; LFIND is 20ns); the nine capacity refusals have no fixtures (stage engine's own cap dies first - needs a direct-drive vehicle)."
created-at: "\"2026-08-11T17:49:45.565263+02:00\""
---

Prerequisite of every baked-code route (Stage B reframe on habu-seed-the-chain-e98b03d4): the capture format is u16 everywhere - call-site rows (blob-off u16, name-off u16) with an explicit boff $FFFF > die at aot-capture.f:125, DATA-site/CODE-site/window-XT offsets all u16 = a hard 64KB blob ceiling against the chain's 1.15MB. Plus capacities: AOT-BLOB-CAP 64KB, AOT-REC-MAX 256 (chain needs ~6554), AOT-NAMES-CAP 16KB (~51KB needed), AOT-WINDOW:DATA-CAP 64KB, AOT-SITE-MAX 2048, AOT-DSITE-MAX 512, XTOFF-MAX 64; and 45 chain records have names > 16 bytes = EXT-name records, which capture refuses outright (aot-capture.f:172). Widen u16 -> u32 across the four tables AND the boot-side walkers in habu2.f, lift the capacities, accept EXT names. Acceptance (falsifiable): capture and boot a blob larger than 64KB. Unpriced number flagged by the design lane: EM-AOT-PATCH-SITES at chain scale (tens of thousands of LFIND-per-site) - probe by synthesizing a blob with N sites and timing the seed pass against N; a snapshot restore of 15.8MB/5839 records boots in 0.015s as the proxy bound. Files: src/habu/aot-capture.f, src/habu/habu2.f, src/habu/habu1.f (labels). Depends: habu-aot-has-no-0b01043c (widening without the pre-window ruling builds a format for a capture that still dies by name).

FORMAT CONSEQUENCES FROM THE PRE-WINDOW RULING (2026-08-12, prewindow lane):
NO third DATA band - the rebased/b0-relative split stays; there is no
correct third delta (host/target prefix layouts are not order-isomorphic).
DO add a name-keyed CODE-site row kind in this same pass: a pre-window
[']/postpone target is exactly a call target that is not a BL, and the
mechanism exists (ACAP-TGT>REC xt->record, call-site table blob-off ->
name-pool ref, EM-AOT-PATCH-SITES LFINDs at boot) - cheap now, a second
migration later. A name-keyed DATA row is NOT available (a create word's
dict record carries no body address; recovering one would need decoding
the chain, which layout.f forbids by name, or an unverifiable blr probe) -
pre-window DATA is eliminated by the inliner decline instead (0b01043c).
Window sizing unaffected today: window DATA 5724B vs DATA-CAP 64KB, blob
18060B vs BLOB-CAP 64KB - the chain is what blows them (1.15MB).

Claim: agent=widen workspace=.jj-ws/habu-widen-aot

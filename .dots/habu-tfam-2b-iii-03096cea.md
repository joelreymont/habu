---
title: "TFAM 2b-iii: seal FORGET/HIDE/USIGS-TRUNCATE with a sealed watermark"
status: open
priority: 2
issue-type: task
created-at: "2026-07-05T00:48:58.601374+02:00"
---

FORGET-DEFS-FROM (src/habu/xref.f:263), HIDE-DEFS-FROM (xref.f:256), and CHECKER-USIGS-TRUNCATE-FROM (src/core/checker.f:3577) can retire/truncate dictionary records and checker signatures back past the seal boundary. They resolve a name to an index then ndict!/cp!/USIG-truncate to it; a name resolving to a sealed system record's index truncates past the seal, retiring sealed defs. Existing address-band guards do NOT catch this: XREF-RETIRE (xref.f:221) uses patch32 into the DBASE record region (PROT-GUARD only bands the friend arena + PROT-WID registry, not DBASE); FORGET's cp! (BCPSET) and ndict! (BNDSET) target the DBASE code/record region, outside both guarded bands. Confirmed empirically: 0 FORGET-DEFS-FROM dies rc 76 (XREF-REQUIRE-INDEX on empty name) - not itself a breach, but a name resolving to a pre-seal record would be. FIX: add a sealed-ndict + sealed-USIG watermark (value cells, not the boolean latch) captured at SEAL-FRIEND; FORGET-DEFS-FROM/HIDE-DEFS-FROM/CHECKER-USIGS-TRUNCATE-FROM reject once sealed (E-SEAL-VIOLATION 83 / E-SEAL-PACKAGE 84) any target index/mark below the watermark; friend/cold-load exempt (latch 0). Red forge: FORGET/HIDE/undefine of a name resolving to a pre-seal record -> reject. Positive: FORGET/HIDE of a post-seal user mark still works (mirror seal.f SLV-FORGET-FORGE round-trip). New watermark mechanism analogous to the friend latch; independent of the protected-WID registry, so implementable now. Cat 2 truncation/bulk-retire portion.

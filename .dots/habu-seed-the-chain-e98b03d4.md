---
title: Seed the chain behind one prefix require
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T18:56:46.248678+02:00"
---

Stage B: append 'require src/compiler/native/migrate.f' into the prefix buffer via the existing C-SOURCE-APPEND-X4-TO shape (PFX-APPEND-ENGINE-SNAP-HOOK precedent) - ONE row, ~40 bytes of IBUFSZ; the closure loads through include.f's own buffers (53 flat rows = 87.5% of IBUFSZ, forcing 4->8MB - the shape is forced, not preferred). Cost: +1.24s per cold bin/hb, +7464 dict records, +1.2MB code, +1.5MB DATA. BLOCKING PRE-MEASURE: count the full gate's cold bin/hb execs so the wall-time multiplier is known before landing. docs/bootstrap.md gains the sentence: gforth stays a pre-chain recovery host and stays correct only while the engine can compile without the chain. Acceptance: boot ndict 13,2xx; NMIGRATE:DEFINE works at first user token; full gate green; byte fixpoint. Files: src/habu/habu2.f. Depends: habu-seed-the-stdlib (Stage A, use minted id), habu-key-the-fixpoint (Stage C, lands with or before).

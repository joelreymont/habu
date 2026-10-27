---
title: Land the incremental-build stack on master
status: closed
priority: 2
issue-type: task
created-at: "2026-10-23T20:20:27.444602+03:00"
closed-at: "2026-10-27T05:44:04.237018+02:00"
close-reason: "landed on master 9f905e31: full suite 503/503 (477 s), two-generation gen2..5 byte-equal, landing review ACCEPT; landed after 087ee3fa and 91788cad"
---

Master moved 63 commits since fork wwkpzwot. Duplicate of wwkpzwot..wvxplqoy placed on master (originals stay under running workers). Piece 1 = 7 clean commits vlmqvrqw..mqnpkktp, gating in carl-land1 (G ~/.cache/tmp/carl-gate1.clZa). Piece 2 = mqnpkktp::nlxprtrw, conflicts (stack e2e and dissolved docs deleted on master) resolved by worker in carl-land2 (brief brief-land2.md). Then add folds (img3 fold oknslpwn, release mappings tlkkkpqz, SVX comment), gate, push.

Joel (2026-10-26): the stack lands only after habu-check-typed-storage-087ee3fa and habu-scan-only-unchecked-91788cad, so that ffs.f stores instead of refusing raw-cell. Stack tip on top of ulxuqysn: lanes CWORK (carl-cwork), refusal triggers (carl-census), FFI boundary tests (carl-rebase).

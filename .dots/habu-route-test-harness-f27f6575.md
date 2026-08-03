---
title: Route test-harness code rewinds through CODE-RECLAIM
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T22:01:07.697916+02:00"
---

src/habu/xref.f now owns CODE-RECLAIM:TRUNCATE, the one checked word that lowers the code pointer and tells every holder of an address-keyed fact (NPUB's claimed-slot line, NCLOB's clobber rows, NINL's recorded bodies) the floor first. Both production rewinds go through it (xref.f FORGET-DEFS-FROM, src/core/generated-declaration-dictionary.f ROLLBACK), and so does test/compiler/native-publish.f ROOM-CASE. Four test-harness rewinds still call the raw cp! primitive: test/prop-test-core.f FORGET/SFORGET/CHK-FORGET (lines 183/185/187), test/gate-common-lib.f GE-EVAL-FORGET (line 466) and test/engine-suite.f ES-HIDX-ROLLBACK-CHURN (line 1806). They are inert today - none of those processes loads the native chain, so there are no watchers and no rows to go stale - and NPUB:SLOT-CK turns a future bypass into E-NPUB-SLOT rather than a miscompile. They were left alone because all three words are unpackaged globals, so editing them trips package-diff-lint E-PACKAGE-OWNERSHIP; route them once those files have package owners.

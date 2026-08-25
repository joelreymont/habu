---
title: Package the snapshot trailer geometry constants
status: open
priority: 3
issue-type: task
created-at: "2026-08-05T02:12:47.590096+02:00"
blocks:
  - habu-give-layout-f-315df2ca
---

The proofs/master merge added seven constants to src/habu/layout.f - SNAP-TRL-BYTES, SNAP-TRL-TBASE, SNAP-TRL-NDICT, SNAP-TRL-REGLEN, SNAP-TRL-DATALEN, SNAP-TRL-VERSION, SNAP-TRL-LEGACY-BYTES - as the single owner of the snapshot trailer's size and field offsets, because the writer (src/habu/snap-lib.f), the loader (src/habu/habu2.f EM-SNAPSHOT-RESTORE) and three readers (tools/imgdump.f, test/snapshot-writer.f, tools/build-fixpoint-test.f) had drifted apart: the readers kept the legacy 40-byte size after the format grew to 48 and addressed the wrong cells while still finding plausible values. layout.f has no package owner, so tools/package-diff-lint.f reports each of the seven as E-PACKAGE-OWNERSHIP; they are the same class as the PROT-* bitmap renames already recorded in habu-pkg-the-protected-0416ce18, and the file cannot join a package until habu-give-layout-f-315df2ca lands (itself blocked on habu-add-using-to-d815f0ab, stage0 has no 'using'). Fold these seven names into that packaging pass; nothing else is needed here.

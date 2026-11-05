---
title: Find why a refused body thrown out of a build sets bit 62 of a header cell
status: closed
priority: 2
issue-type: task
created-at: "2026-11-05T23:20:37.939713+02:00"
closed-at: "2026-11-06T01:08:33.088013+02:00"
close-reason: "Cell $169638 is in the address-literal map (layout.f SNAP-RELOC ADDRMAP): habu2.f EM-EVAL-THROW-RECOVER rewinds CP without clearing ADDRMAP/CALLMAP over the span, so a created body's MOVZ bit (NPUB RELOC-ADDR) outlives the throw. Machinery being replaced: the case joins habu-clear-the-addr-23daaaee in habu-move-the-interpreter-d35780fa; no habu2.f patch. The refused-definition HOST-CHECK case comes with S5b2-1."
---

Problem: with S5b1b's WITHOUT-PACKAGE removed (the old LINKER:WITH), test/linker.f case k's body is refused rc 70 and its throw out of the build leaves hb's DATA header cell at offset $169638 with bit 62 set (HOST-CHECK, HEADER-DIFFS 1, /tmp/jerry-s5b1b/mutation.log). The old WITH never touched package cells, so another store leaks on that throw path. Acceptance: name the cell and its owner; a case in test/linker.f whose build body the checker refuses (e.g. a definition with a wrong effect, under the new WITH) shows HOST-CHECK clean, or the owner's restore is fixed. Files: test/linker.f, src/habu/linker.f, the owner of the cell. Verify: bin/hb --load test/run.f -- linker. Depends: S5b1b landed. Ownership: jerry. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-hdr (hunt brief /tmp/jerry-hdr/brief.md).

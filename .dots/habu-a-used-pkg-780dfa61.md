---
title: A used-package name costs a whole dictionary scan
status: open
priority: 3
issue-type: task
created-at: "2026-08-04T10:48:00.033405+02:00"
---

Full context: EMIT-FIND-USED (LFINDUSED) in src/habu/habu2.f resolves a bare token through the packages a file has imported with using, and it does it with one pass over every dictionary record from index 0 to NDICT, testing each record's wid against the live USE-WIDS[0..depth) and comparing the folded name. It runs only after the open-scope and global chain has missed, which since dot habu-compile-shaped-cost-4e74a181 is a handful of hash probes rather than two full scans - so in a file with a using open, the scan LFINDUSED does is now the whole cost of resolving an imported name: about 8 us at ndict 12000, against 1.05 us for a name the global probe finds. The single pass exists to detect AMBIGUITY - a bare tail resolving in more than one used package is E-USING-AMBIGUOUS and must be a hard error, so the search cannot stop at the first hit. That is preserved by probing the hash index once per used wid (depth is bounded by USE-MAX, 16) and counting the hits: the same two-match rule, over at most 16 probes instead of NDICT records. Keep the scan for no-table and chain-exhausted, as LFIND does, and pin the ambiguity error and the used-private invisibility with fixtures before removing it - test/using-test.f is where they belong. Depends: src/habu/habu2.f EMIT-FIND-USED; src/habu/habu1.f C-HIDX-HASH; test/using-test.f. Ownership: using-scope resolution. Claim: unassigned.

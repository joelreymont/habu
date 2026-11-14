---
title: Key the dictionary and checker symbols on interned names
status: open
priority: 2
issue-type: task
created-at: "2026-11-03T18:09:53.296758+02:00"
---

Joel 2026-11-03: 'yes, the other 2 hash tables should use the string address instead of hashing again' and, on merging them, 'yes if they both point to the same type of data'. Today the engine's dictionary index (src/habu/habu1.f C-HIDX-HASH :3872-3883 (WLFIND :3903) FNV-1a over the A-Z-folded name, src/habu/layout.f HIDX) and the checker's symbol hash (src/core/checker.f SYMS, symbol hash index :7969) each hash a name's bytes; dictionary records keep their name inline or by DNAME-EXT pointer, not in INTERN. Goal: a name is interned once (INTERN:STRING) and both tables key on that address; where both map a name to the same kind of record they become one table. Open for the design: case folding (the dictionary folds A-Z, INTERN is byte-exact), records captured into images and snapshots (addresses relocate), rewinds (seed-ndict!, FORGET, a failed REPL line discards its strings), T0/x86/Wasm twins. Acceptance: a design the critic accepts and Joel approves, then lanes. Files: src/habu/habu1.f, layout.f, xref.f, src/core/checker.f, src/habu/intern.f. Verify: per lane. Depends: habu-move-the-str-aa9e85fa (INTERN move, jerry). Ownership: carl (design). Claim: unassigned.

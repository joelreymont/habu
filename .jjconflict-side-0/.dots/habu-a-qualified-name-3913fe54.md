---
title: A qualified-name existence oracle for the engine
status: open
priority: 2
issue-type: task
created-at: "2026-08-11T17:13:38.375532+02:00"
---

Found by the primsweep lane (2026-08-11): the engine's only dictionary existence primitive is search-wl, per-wid on the raw spelling - a closed package's publics are findable in NO wid 0..63 (probed: A64ASM:ENC-LDUR and a test package's PQW compile and run yet search-wl finds neither), and a package name is not itself a dictionary word. Consequence: a fail-closed TRUST can verify bare top-level names only; the 28 qualified PKG:TAIL rows in the tree cannot be existence-checked at the row without a new capability - a qualified-find that resolves PKG:TAIL through the package tables the way a reference site does. First consumer: TRUST's fail-closed check extending to qualified rows (cc8e19de lands bare-only). Files: src/habu/habu2.f (or wherever find lives), bootstrap/cg/forth.fs mirror, src/core/checker.f. Depends: none.

---
title: "C2c2-table: hb's own text table, INTERN:TEXT, FIND, KEEP-BELOW"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-07T00:44:54.150893+02:00"
closed-at: "2026-11-07T02:23:51.606011+02:00"
close-reason: "implemented, reviewed (code-reviewer: six claims hold), merged: chain gen2 fixpoint (/tmp/jc.9u, 431.8 s for three builds at load 40-50), full suite on gen2 293/293 PASS (123.9 s)"
---

Problem: the facts writer (C2c2-write) needs one table outside a build as inside one: src/habu/intern-table.f serves only a build's space (SPACE-STRING); hb's literals use intern.f's old pool. A failed REPL line's texts must go with DATA's rewind (Joel 2026-11-03: "any strings stored during a failed repl line should be discarded"). Acceptance: a table is a three-cell record (slots address from NULL-PTR or 0, slot count, filled); SPACE-TABLE the space's, OWN-TABLE hb's (first cells PROCESS-CELL); public INTERN:TEXT ( ptr u8 n -- n ) (space: SPACE-STRING's refusal then find-or-lay; outside: hb's table), INTERN:FIND ( ptr u8 n -- n ) (0 for none, same refusal in the space), INTERN:KEEP-BELOW ( n -- ) (rebuild keeping bodies below n, sharing one rebuild word with growth); callers interpret.f FRAME-ROLLBACK-CODE (cut) and generated-declaration-dictionary.f DICTIONARY-DP! (stored DP); PPRIM rows; error.f:849 comment states the build rule. Proof: candidate build; pty REPL probe (failed line forgets, 3000 kept bodies survive the rebuild, a good line keeps its text); tests engine linker file-objects repl-address-cell-rollback. No suite test: the REPL e2e case lands with C2c2-write. Files: src/habu/intern-table.f, src/habu/intern.f, src/core/cell-effects.f, src/habu/interpret.f, src/core/generated-declaration-dictionary.f, src/lib/error.f. Verify: brief /tmp/jerry-big/brief-c2c2-table.md. Depends: C2c2a (habu-c2c2a-move-the-34ad64fd). Ownership: jerry. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-c2c2t.

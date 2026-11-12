---
title: Delete dead engine words
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:30.800206+03:00"
---

Problem: these words have no caller in src, tools, test or ../etch (rg on master): src/habu/rt.f:220 EMIT-DOT and :250 EMIT-ATOI (rt.f itself is loaded by src/arch/arm64/boot.f:20); src/habu/xref.f:152 XREF-WIDE? and :749 INVALIDATE!; src/habu/address-cells.f:90 WITH-HOST-HEADER; src/habu/layout.f:911 DECL-SOURCE-ROW-OFF, :912 DECL-SOURCE-CON-OFF, :914 DECL-WIDE-OFF, :956 DECL-TRUST-DECL-OFF, :1420 LOOP-STK-FRAMES and :1887 SX-HANDLER; src/habu/task-abi.f:16 THREAD-OFF, :26 RET-OFF and :27 USER-XT-OFF, though that file states the C-to-Habu task entry contract, so the C side must be checked before they go; src/core/util.f:89 the variable RDP. src/habu/bundle-argv.f is never loaded (only tools/lint/shadow-lint.f:197 lists it) and duplicates src/os/script-argv.f. src/core/roles.f:183-194 VRDEF-FOLD-C and VRDEF-STR=CI duplicate src/core/util.f:47-54 CORE-FOLD-C and CORE-STR=CI, which load first. Words in habu1.f, habu2.f and jit.f are left out: they are machinery being replaced (AGENTS.md "Machinery being replaced").

Acceptance: each word above deleted, or kept with the caller found, after rg shows no caller in src, tools, test or ../etch; bundle-argv.f deleted and dropped from shadow-lint.f's list; roles.f uses util.f's two words; hb's behaviour unchanged apart from the removed words.

Files: src/habu/rt.f, src/habu/xref.f, src/habu/address-cells.f, src/habu/layout.f, src/habu/task-abi.f, src/core/util.f, src/core/roles.f, src/habu/bundle-argv.f, tools/lint/shadow-lint.f.

Verify: the candidate engine built as docs/bootstrap.md "Refresh `bin/hb`" says (`bin/hb --load tools/hb-build.f -- --engine -o $OUT/hb`), `test/run.f` on it, and the convergence check `tools/chain-run-build.f` (docs/bootstrap.md "Convergence check"), since the engine's bytes change.

Depends: none.

Ownership: unassigned. Claim: unassigned.

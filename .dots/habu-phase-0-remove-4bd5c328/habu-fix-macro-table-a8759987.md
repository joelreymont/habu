---
title: Fix macro-table GC keying
status: closed
priority: 1
issue-type: task
created-at: "\"2026-04-01T22:06:02.065190+02:00\""
closed-at: "2026-04-01T22:35:26.029707+02:00"
close-reason: "done: compiler O(n) macro-name scans removed; REPL package-name macro fallback removed; macro-table access now rekeys through live-key helpers and restore paths; validation: zig build test back to existing 5-error baseline (disasm missing opcodes + builder.lambda arity mismatches)"
---

Problem: macro lookup keys go stale across moving GC and fall back to O(n) name scans. Acceptance: macro tables use stable keys or correct rekeying and lookupMacroByName fallback is deleted. Files: src/interp/repl.zig:4209-4255, src/interp/vm.zig, src/runtime/gc.zig. Verify: large-load macro regression and rg for lookupMacroByName fallback. Blockers: none.

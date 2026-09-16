---
title: Write the prefix load table once in habu2.f
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T16:25:10.794150+03:00"
---

Problem: src/habu/habu2.f carries the same 58-row (kind, label, path) prefix table three times, as PFX-LOAD-* (about lines 797-1005), PFX-PATH-* and PFX-PROVIDE-* (about 1332-1415), 58 row calls each, and a normalised diff (Opus audit 2026-09-16, scratchpad audit-bloat-core.md cut 1) shows the copies have already diverged by five rows. Acceptance: one table of rows, each row carrying every attribute the three consumers need, with the three emitters walking it; the five divergences resolved deliberately with the reason for each in the commit; the emitted prefix byte-identical to the current one where the divergence was accidental (compare the cold prefix source text before and after) or the intended fix named; engine byte fixpoint; stage0 chain OK (bootstrap/cg/forth.fs mirrors the prefix rows, keep it in step); full gate green. Files: src/habu/habu2.f, bootstrap/cg/forth.fs. Verify: prefix text diff; tools/native-build.f fixpoint; tools/bootstrap.sh; test/run.f. Depends: none. Ownership: engine prefix emitter. Claim: unassigned.

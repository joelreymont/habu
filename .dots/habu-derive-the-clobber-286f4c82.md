---
title: "Derive the clobber lint's hand models of emitter words"
status: open
priority: 3
issue-type: task
created-at: "2026-11-01T05:06:55.882481+02:00"
---

Problem: tools/lint/clobber-lint-core.f models emitter host words by hand: PSEUDO-EFFECTS (mark-save, mark-restore, c-lit, c-call, g-push, cf-entry and the rest) and CLOBBER-WRAP (PROT-GUARD:CALL, C-FIND-GLOBAL) state each word's register reads and writes in a table that must track the word's code. Lane sealed's fold 4c derives a summary for every other host word from its own definition. Acceptance: each hand-modelled word defined in a scanned file uses its derived summary, its hand entry goes, and the clobber-lint row reports no new finding (or each new one is judged). Files: tools/lint/clobber-lint-core.f, tools/lint/clobber-lint-test.f. Verify: row clobber-lint. Depends: habu-answer-tools-check-745b0ccd (fold 4c lands the summaries). Ownership: tools/lint/.

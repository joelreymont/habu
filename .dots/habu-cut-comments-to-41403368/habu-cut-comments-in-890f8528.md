---
title: Cut comments in src/compiler
status: closed
priority: 2
issue-type: task
created-at: "2026-10-29T21:49:33.344753+02:00"
closed-at: "2026-10-30T00:34:24.397471+02:00"
close-reason: landed as master 18f0605d with the comp6 fold-in cuts, reviewed LAND, code byte-identical
---

Problem: the comments under src/compiler restate code, narrate history or name deleted words; rule and outcomes in the parent and /tmp/jerry-big/t0-comments-brief.md. Acceptance: one comment-only commit for src/compiler (code lines byte-identical), every kept comment a fact the code cannot state, headers a short map, no named word missing from the tree; reviewed by a code-reviewer against the rule and landed. Files: src/compiler. Verify: the comment-only diff filter prints nothing; the names check finds every named word; hb --load tools/check.f on each file where the engine does not provide it. Depends: none (the worker takes the directories in the parent's order). Ownership: comments under src/compiler. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-t0-comments.

Batch 2 (txpzkrxl, on ypyowpqq) complete 2026-10-29: every src/compiler file read, ten left unchanged; code identical except three trailing comments cut from hir-word.f KEY-A/KEY-Z/KEY-BIT lines; stale FLOW-MEET/FLOW-DEF mentions and two wrong claims (select-x64.f E-X64SEL-TRAP, select.f COMPARE-COND) corrected. Under code-reviewer review.

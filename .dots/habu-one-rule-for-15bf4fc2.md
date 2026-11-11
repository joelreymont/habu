---
title: One rule for interpreter keywords as names
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T14:27:50.770438+02:00"
---

Problem: src/habu/interpreter.f DEF-INTERPRET-KEYWORDS (1667) are refused as definition names only under a sealed design, so ': create', ': variable', ': constant', ': package', ': public', ': private', ': ;package', ': seal-package', ': export', ': char', ": '", ': trusted:', ': kernel:' are admitted, but a top-level use runs the interpreter's own word while a body call runs the user's (hunt 633076da, reach.tsv). One name, two meanings by position. Acceptance: one rule: the definer refuses these names always (DEF-WALL), unless the census of Zen, Tender, Maki and Habu's own sources finds a definer of one, in which case the lead decides with that list. Files: src/habu/interpreter.f, habu2.f twin if still run, docs/forth.md 'Rules learned by refusal'. Verify: each name refused by bin/hb --load; test/engine-suite.f ES-CHAR-N (550-551) cases rewritten to names the language admits. Depends: none. Ownership: interpreter.f DEF-WALL. Claim: unassigned.

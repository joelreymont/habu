---
title: Write a long string literal across lines
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T12:30:45.731536+02:00"
---

Problem: docs/forth-card.md:139 limits a line to about 60 columns, and a string literal longer than that cannot meet it: s" and s\" read to the closing quote on one line, and Habu has no way to continue a literal on the next. 5,674 lines in the tree hold a literal too long for 60 columns however the line is wrapped (TREE at test/browser/navigation.f:85, 117, 128; T-LABEL in test/browser/navigation-test.f; SVG in test/browser/display.f among them). Proposal shown to Joel 2026-11-12, not approved: a word that continues the literal before it, joined at compile time, so the program holds one string and pays nothing at run time: s" a message that would not fit in " +" sixty columns" type; a second form after s\". The name is Joel's to pick. Acceptance: a literal of any length is written on lines within the limit and compiles to one string; the word's name and syntax approved by Joel. Depends: the release (Joel 2026-11-12: 'let's dot and drop this for the time being. my goal is to have well-factored words but we can delay this until after release'). Claim: unassigned.

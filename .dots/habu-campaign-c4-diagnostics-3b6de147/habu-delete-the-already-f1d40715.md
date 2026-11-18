---
title: Make every engine throw a rejection, a die or a caught code
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T16:00:42.384409+01:00"
---

Problem: a throw in src/core, src/habu or src/compiler, src/habu/kernel-x64.f included, that is not a rejection carrying its record, a 76 die, or a control code a named catcher in its package handles reaches a root as its code's generic row record; and a caught code thrown from a unit other than its catch's is a new throw that drops the record it carried (docs/forth-card.md :608-617). Acceptance: a census lists each throw of the three directories outside those classes, with file:line, and each is made one of them; the x86-64 kernel's changes are carl's. Depends: none. Claim: unassigned.

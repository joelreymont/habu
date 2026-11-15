---
title: Bar every declaration word
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T22:22:24.205509+01:00"
---

Problem: ': foo ( -- ) HEAP ;' and the same with ARENA, STRUCTURE, DEFTYPE and TASK:+USER load in a checked body: they are declarations to DECLARATION? (src/habu/interpreter.f:2610) and RECORD-DEFINER? (src/habu/verify-source.f:3496) but missing from UNSAFE-TOK?'s list; three lists of declaration words drift apart. Acceptance: each is rejected in a checked body, and the bar and the declaration routing read one list. Depends: habu-bar-a-declaration-1ae5a61c. Claim: unassigned.

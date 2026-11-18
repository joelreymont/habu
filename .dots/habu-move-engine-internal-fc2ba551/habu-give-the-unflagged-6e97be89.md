---
title: "Give the engine's dictionary writers private rows"
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T12:38:59.423532+01:00"
---

Problem: ndict!, cp!, prot-wid-add and record-facts! keep a global row with no mark (src/habu/prims.f), so a checked file's top-level line runs them: `99999999 ndict!`, `999999 prot-wid-add` and `0 99999999 record-facts!` reach kernel stops on the installed hb, and `99999999 cp!` exits 0 with the code pointer past the data region. docs/forth-card.md (a task's compile rules) and docs/forth.md (a `does>` head's rules) name cp! and ndict! as a program's words. Acceptance: each has one private row per calling engine package and no global row; each of those lines is E-UNDEFINED under tools/check.f -- --verify-only and when it runs; the docs name neither as a program's word; a test that calls one directly calls its package's word instead. Depends: none. Claim: unassigned.

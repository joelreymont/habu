---
title: Give the unflagged engine primitives private rows
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T12:38:59.423532+01:00"
---

Problem: ndict!, prot-wid-add, record-facts!, ptr-cell-mark and map-anon keep a global row with no mark (src/habu/prims.f), so a checked file's top-level line runs them: `99999999 ndict!`, `999999 prot-wid-add`, `0 99999999 record-facts!` and `4096 map-anon drop ptr-cell-mark` reach kernel stops on the installed hb. Acceptance: each has one private row per calling engine package and no global row; each of those lines is E-UNDEFINED under tools/check.f -- --verify-only; a test that calls one directly calls its package's word instead. Depends: none. Claim: unassigned.

---
title: Structure image dump dictionary rows
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T21:39:39.924253+02:00"
---

Problem: tools/imgdump.f:48-51 decodes each dictionary entry into four parallel arrays (name start, name length, start, code length) with eight field accessors (:280-287), so a swap of two same-width fields passes the checker and can report two dictionaries equal or attribute a program counter to the wrong word. Acceptance: one STRUCTURE for a dictionary entry (name span, start, length) holds the decoded rows, decoded from the image's bytes in one place; dump and compare output stays byte-identical; a swapped field is rejected by the checker. Depends: none. Claim: unassigned.

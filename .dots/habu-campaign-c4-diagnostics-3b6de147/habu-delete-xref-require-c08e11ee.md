---
title: Delete XREF-REQUIRE-INDEX with its x86 callers
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T06:35:37.151094+01:00"
---

Problem: src/habu/xref.f:380 XREF-REQUIRE-INDEX dies on a name its only callers (test/x86-64-kernel-prof.f:103-107) define themselves; deleting it means editing that x86 test. Acceptance: the word, its die and the four calls are gone, at the x86-64 step. Depends: the x86-64 step. Claim: unassigned.

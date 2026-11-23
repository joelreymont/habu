---
title: "Keep one view check: C2-MEM's views or the vector-view check"
status: open
priority: 2
issue-type: task
blocks:
  - habu-reject-a-vector-0feb8b8f
created-at: "2026-11-23T19:38:35.629560+01:00"
---

Problem: the checker has two mechanisms for pointers into owned memory: C2-MEM's read-view and mut-view (src/lib/c2-memory.f, docs/forth.md:1935-2046), which no production file in Habu, Zen, Tender or Maki uses, and the vector-view check (.out/designs/design-vecview.md, dot habu-reject-a-vector-0feb8b8f), which the SB replacement needs. Acceptance: one mechanism remains; what the other provided is either covered by it or deleted. Depends: habu-reject-a-vector-0feb8b8f. Claim: unassigned.

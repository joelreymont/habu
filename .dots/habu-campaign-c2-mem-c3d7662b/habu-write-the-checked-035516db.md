---
title: Write the checked ownership model
status: open
priority: 1
issue-type: task
created-at: "2026-09-16T13:54:54.703368+03:00"
---

Problem: six memory-safety dots (scoped memory, read borrows, mutable scratch borrows, region-typed pointers, linear capture phases, fixed DATA layout) each define their own vocabulary, so lanes would implement three rule sets; docs/forth.md and docs/type-system.md say nothing about lifetimes. Acceptance: a section in docs/type-system.md, referenced from docs/forth.md, defining region, owner, read borrow, mutable borrow, escape, phase and release with their checker rules and the diagnostics each violation produces, illustrated with Tender's OPC buffer list rewritten on the typed surface; the six dots are amended to cite the section and their public words are renamed to it where they differ. Files: docs/type-system.md, docs/forth.md, the six dots. Verify: doc review against the current checker; each dot's Acceptance names the section. Depends: none. Ownership: checker lane. Claim: unassigned.

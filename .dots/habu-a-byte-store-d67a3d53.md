---
title: A byte store through byte-view corrupts a pointer cell
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T08:28:00.866204+02:00"
---

Problem (recorded open at docs/forth.md:1402-1406, confirmed by /home/joel/Work/habu/.jj-ws.noindex/jerry-wasm/.out/design-null-reach.md): a byte store through the byte-view of a cell that holds a pointer stays in bounds yet makes the cell a pointer to anything, which the checker then admits as a ptr. Acceptance: a checked program cannot change a pointer cell's bytes except by storing a pointer; a rejected program for each route. Depends: none. Claim: unassigned.

Acceptance case (2026-11-12): the same byte-view route (byte-view cell-view @ and !, and byte-copy between two cells' byte views) retypes a cell holding a quotation, not only a pointer cell.

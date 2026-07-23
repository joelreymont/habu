---
title: "Infer alloc: class contract table"
status: open
priority: 1
issue-type: task
created-at: "\"2026-07-22T09:43:30.659519+02:00\""
blocks:
  - habu-type-dsl-prove-93da83c4
---

Problem: eight allocation classes need one executable contract for ownership,
lifetime, alignment requirement, CPU and GPU access, synchronization, cleanup,
and accounting. Rejected commit 2a991843 duplicated that authority across two
legacy PRODUCT types, seven draft enums, three validation matrices, eight
builders, and test replicas; its public complete-record constructor also
accepted contradictory contracts.

Required result: after the unified type cutover, package ALLOC-CLASS publishes
one payloadless kind ENUM with the eight first-release classes. A private
STRUCTURE contract contains typed fields for the seven policy domains plus an
alignment-requirement ENUM. One exhaustive function maps each kind to exactly
one canonical contract. Public read-only queries consume kind and return one
field; no public raw constructor, draft type, mutable table, numeric alignment,
allocator choice, backing choice, or duplicate validation matrix exists.
Missing ENUM arms fail checker certification; duplicate variants fail the
declaration transaction. The backing-policy planner consumes these queries
through its real planning path, and the focused test is enrolled in Maki's
canonical suite.

Acceptance: all eight kinds reach one canonical row; a mutation removing any
kind fails exhaustive checking, a duplicate variant rejects, and no external
checked source can construct or mutate a contract. The planner-path test proves
every query is consumed and that conflicting policy cannot be represented.
Source and tests contain no PRODUCT, SUMTYPE, raw role indices, magic alignment
bytes, or copied table. Unified type, package, Maki, and file-map gates pass.

Files: maki/infer/allocation-class.f, its focused test, Maki suite inventory,
and FILEMAP.md.
Smallest real check: the enrolled Maki allocation-planner slice.
Depends: habu-type-dsl-prove-93da83c4.
Owned result: allocation-class contract and queries only; backing selection
remains habu-infer-alloc-backing-84051fcd.
Claim: released; commit 2a991843 is rejected evidence only.

---
title: "Campaign C2: memory safety the checker can see"
status: open
priority: 1
issue-type: task
created-at: "2026-09-16T13:54:06.629638+03:00"
---

Problem: the checker proves stack effects and nominal types but not lifetimes. Tender's OPC module frees intrusive buffer lists by hand, Tender's LESSONS record locals overwritten by later pushes, and lib/task.f and the AOT capture path reuse buffers whose authority nothing tracks. Generated code will leak and corrupt exactly there and nothing fails at check time. Acceptance: a program that stores a scoped span past its owner's release, frees while a reader lives, or reuses a phase buffer out of order is rejected at CHECK! with a fixture for each; Tender's OPC lists are rewritten on the typed surface as the proof; docs/forth.md and docs/type-system.md state the ownership model. Children (open): habu-write-the-checked-035516db habu-unify-all-quotation-56884608 habu-own-the-layout-3bd40ca9 habu-make-json-writer-e454cd08 habu-add-unique-bounded-527e05ca . Absorbed on 2026-09-16: 86 dots closed with the reason 'superseded by habu-campaign-c2-mem-c3d7662b'; find their text with dot find. Files: src/core/checker.f, lib/memory.f, lib/memory-region-borrow.f (new), docs/forth.md, docs/type-system.md, docs/roadmap.md section C2. Verify: checker quotation and linear fixtures, memory suites, test/run.f green. Depends: none. Ownership: checker lane. Claim: unassigned. Absorbed: see the archive entries closed with 'superseded by' this id.

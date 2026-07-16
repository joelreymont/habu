---
title: Review shared type-field schema
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-17T03:00:55.146819+02:00\""
---

Independent destruction review of revision a923c606 and dot habu-fields-add-shared-6b063c62. Read the shared-field dot/spec and code only; verify package ownership, unified ENUM field reuse, no public PRODUCT invention, hard-cutover compatibility, native/recovery/AOT/fixpoint parity, generated constructor namespace, checked effects, tests, docs, and absence of unrelated complexity. Read-only review in an isolated jj workspace; report file:line findings and exact missing tests. Do not edit implementation or close the implementation dot.

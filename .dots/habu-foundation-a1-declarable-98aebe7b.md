---
title: "Foundation A1: declarable nominal integer types"
status: open
priority: 1
issue-type: task
created-at: "2026-07-18T14:44:29.489629+02:00"
---

MISSING.md Foundation A remaining half (its dot-mapping table is stale; the old c2 dot is gone — this supersedes). Depth-review facts: checker.f CT registry is ALREADY data-driven (CT-INIT rows via CT-SET, CT-SNAPSHOT-PERSIST exists) — A1 is declarability, not restructure. Scope: (a) declaration via the unified type DSL (MISSING.md:197), package-scoped, minting role codes past CC-MAX with CT-SET semantics; (b) unification rules: declared role vs same-role accept, vs other-role reject, vs generic-int accept — extending existing CT-ROLE class handling; (c) snapshot/AOT/fixpoint persistence of declared rows + rollback transactionality matching the PF idiom; (d) negative fixtures per rule + byte-fixpoint + full gate. TRIPLE critical path: MISSING ergonomics + maki EXTENT:/TENSOR: accessors (habu-extent-typed-tensor-bde435dc) + SPEC: default golden surface (habu-spec-word-generating-0729fbea). First consumer & acceptance vehicle: EXTENT:/TENSOR: with idx<#extent> sigs. SEQUENCING: after fields lane settles sumtype.f grammar (factor-field-schema); checker split (habu-split-checker-f-837bc1a4) and generic registry (habu-generic-registry-word-09088e38) ride this dot's churn window. Avoid collision with active sol lanes (protect-type-field, nominal-storage-migrate).

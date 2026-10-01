---
title: Prove product identity across host engines
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T23:47:27.342503+03:00"
---

Problem: the product must not depend on the host engine that builds it (closed habu-make-the-product-bed415cf), but no check builds the same tree under two host engines that differ by a primitive. Astra (PD review, `lvtnvxqu`) found the gap; Fable flagged a suspect: src/core/generated-declaration-dictionary.f `SNAPSHOT` writes `ndict@` into `FRAME-BOOT`, a record count that is the host's (docs/bootstrap.md "A record number is the host's").
Acceptance: an E2E check builds the product with the shipped engine and with an engine from a tree with one extra primitive, and the two products are byte-identical; the `SNAPSHOT` cell is shown host-independent or fixed. Fresh builds in a reviewer's environment hit `E-NCOMP-OWNER` (-8574) after adding a primitive: reproduce and fix or explain it first.
Files: src/core/generated-declaration-dictionary.f if the suspect holds; a check under test/ run by bin/hb.
Verify: the new check on spark; chain.
Depends: none.
Ownership: krait.
Claim: unassigned.

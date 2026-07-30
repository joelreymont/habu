---
title: Convert the type declaration surface
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T16:15:45.305417+02:00"
---

Campaign (Joel, 2026-07-30): stop-the-world conversion, no gates until complete, one landing at the end. End state: STRUCTURE + ENUM + carrier-form NEWTYPE (NEWTYPE idx n - stated carrier, distinct nominal, derived converters) + CONSTRUCT owner + DERIVE eq/hash. Deleted: SUMTYPE, PRODUCT, DEFTYPE, the old NEWTYPE arity grammar, proof-token ceremony. Split: codex owns the engine half (grammar, definer, namespace nesting, name-limit rejection); claude owns the tree migration sweep (every declaration, call site, test, doc). Sequencing: engine half -> one fixpoint refresh -> migration sweep -> single full gate battery -> one landing.

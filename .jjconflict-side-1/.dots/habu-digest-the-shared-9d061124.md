---
title: Digest the shared interning vector table
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T10:20:02.316870+02:00"
---

Full context: test/compiler/ir-intern-schema.f freezes its intern sequences by role, interner-kind and declared-key coverage. That catches a DELETED sequence (proven by falsification) but not a WEAKENED one: shortening a covered sequence — say the ceiling sequence from four steps to three — asks both the Habu side and the generated Rocq obligation a weaker question, and both stay green because they move together. Give the table what test/compiler/ir-id-schema.f gives the identity vectors: emit canonical bytes for every sequence (role, kind, ceiling, and each step's key, ordinal and throw code), hash with SHA-256, and compare against a committed frozen digest, so weakening a row requires a conscious re-freeze. Reuse the canonical byte builder and token alphabet from the identity schema rather than writing a second one — that means extracting it, since it currently lives inside package COMPILER-ID-PROOF.

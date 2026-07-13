---
title: Ratchet primitive effect rows
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T16:05:05.304395+02:00"
---

Full context: the live PES table and prop census exist, but trusted-inventory counts prim-axiom trust classifications rather than the authoritative PRIM:/PPRIM: rows. Fix: add an independent checked inventory ratchet over the live primitive-effect registry with stable per-row identities, duplicate/missing/add/remove detection, and a count separate from TRUSTED site classes. Acceptance: adding, deleting, duplicating, or reordering an identity without an explicit migration fails; baseline and strict reports name the exact row; permanent trust owners and primitive rows remain distinct. Files: tools/trusted-inventory.f, tools/trusted-inventory-test.f, docs/effects.md, TRUSTED.md ratchet metadata. Verify: trusted-inventory test, strict/baseline, trust-lint, host/filemap/status lints, full native gate. Depends on permanent capability-owner registry.

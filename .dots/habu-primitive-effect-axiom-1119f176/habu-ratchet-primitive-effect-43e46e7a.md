---
title: Ratchet primitive effect rows
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T16:05:05.304395+02:00"
---

Full context: the live PES table and prop census exist, but trusted-inventory counts prim-axiom trust classifications rather than the authoritative PRIM:/PPRIM: rows. Fix: add an independent checked inventory ratchet over the live primitive-effect registry with stable per-row identities, duplicate/missing/add/remove detection, and a count separate from TRUSTED site classes. Acceptance: adding, deleting, duplicating, or reordering an identity without an explicit migration fails; baseline and strict reports name the exact row; permanent trust owners and primitive rows remain distinct. Files: tools/trusted-inventory.f, tools/trusted-inventory-test.f, docs/effects.md, TRUSTED.md ratchet metadata. Verify: trusted-inventory test, strict/baseline, trust-lint, host/filemap/status lints, full native gate. Depends on permanent capability-owner registry.

## Preflight refinement (2026-07-13)

Implement parsing and identity in a separate checked `PEINV` package owned by
`tools/primitive-effect-inventory.f`; keep `TINV` trust-site rows and its public
compatibility aliases unchanged. A row identity is the canonical tuple of
`PRIM:` versus `PPRIM:`, defining package, word spelling, normalized complete
effect-token sequence, and flags including `PRIM-TRUSTED-ONLY!`. It is never a
path, line, mutable ordinal, or `PES` address. Manifest order is separately
committed so reorder drift fails, while case/whitespace/comment-only source
changes preserve identity. Cross-check the parsed sequence against live `#PE`,
package/name, arity, and flags row-for-row.

Required fixtures cover multiline rows; ignored comments and strings; same
spelling with different effects or packages; formatting-only stability;
effect/package/spelling/trusted-only mutation; add, delete, duplicate, reorder,
and explicit baseline migration. The default trust TSV remains byte-compatible;
strict/baseline append a separately prefixed primitive summary. Add the new
file to `FILEMAP.md`. Serialize this work after active `TRUSTED.md`,
`docs/effects.md`, and `FILEMAP.md` owners finish; then claim with `dot on`
before creating its isolated worker workspace.

---
title: Ratchet primitive effect rows
status: closed
priority: 1
issue-type: task
created-at: "2026-07-13T16:05:05.304395+02:00"
closed-at: "2026-07-17T06:36:03.226199+02:00"
close-reason: "Landed as 'tools: primitive-effect inventory ratchet' (d90c99cb on d42b1878; fixpoint provably unmoved - tools/docs/tests only). package PEINV (tools/primitive-effect-inventory.f + test, 1397 lines): an independent identity-stable inventory over the live PRIM:/PPRIM: registry, distinct from TINV's trust-site classes. Identity = canonical folded tuple <kind> <defining-package> <spelling> <flags> <normalized-effect-tokens> - never path/line/ordinal/address, so formatting-only edits preserve identity; scan follows boot-pin's prefix load order (checker.f -> sumtype.f -> layout-buffer.f = live table order); baseline = 298 ORDERED manifest lines in TRUSTED.md (ordered not sorted, so reorder is detectable and names the position); comparison is OCCURRENCE-AWARE (multiset). Acceptance proven as a full tamper matrix, each naming the exact row: add/delete/duplicate/reorder/bogus-row/malformed-manifest/effect-package-spelling-flags mutations all fail; explicit migration passes 'ratchet ok'. DISCOVERY recorded and enforced: path0/PATH0 is a genuine committed duplicate axiom (checker.f:4517 + 4585, identical effect twice in the live PES table) - multiset comparison records its exact multiplicity (a third copy fails as excess); a future dedup is a one-line manifest migration. TINV untouched; 6 new prim-axiom classification rows + audit-table rows (owner 1119f176); docs/effects.md 'Inventory ratchet' section; suite wired in both lists. Evidence on exact tree: PEINV test ok; baseline 'ratchet ok 298' + strict 'cross-check ok 298'; TINV test ok; trust-lint 713/739 0 findings; suite-coverage 113/0; typed-local-diff/host/filemap/dot-dep clean; prop census 298 OK; full run.f GREEN marginal-pass 2-of-3. Honest boundary documented in-file: the PRIM-bearing scan list is hardcoded (mirrors boot-pin); a new axiom-bearing prefix file must be added to SCAN-REPO - until then the strict cross-check fails closed on the count mismatch (names a count, not a file). The parent axiom dot 1119f176 remains the owner of the table itself."
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

Claim: agent=peinv workspace=.jj-ws/fable-peinv

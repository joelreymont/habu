---
title: Compile authenticated source frames
status: open
priority: 2
issue-type: task
created-at: "2026-07-16T00:44:24.858224+02:00"
blocks:
  - habu-m2-safe-filesystem-6d289b3d
  - habu-authenticated-source-frame-7c4e1a90
---

Problem: destruction review proved flat textual source concatenation cannot preserve Forth source-frame EOF semantics. Child frames ending in line or parenthesis comments, parse-name, char, tick, variable, constant, package, export, undefine, custom parsing immediates, and pending compiler parsers consume parent bytes or change failure classes; ordinary separator bytes cannot represent EOF because parsers skip or consume them. Implement the canonical authenticated frozen-source provider: hb-build supplies the frozen entry and dependency byte table plus content identities; normal include/included/require/required/provided resolve only through that table; each authenticated table entry opens through `habu-authenticated-source-frame-7c4e1a90`, whose sole frame stack enforces current-frame limits and exact parent restoration; no child can open mutable filesystem content after freeze. `habu-stable-source-origin-frame-9d4b2a61` captures immutable provenance directly from that substrate; neither provenance records nor frame lifecycle live here. Absolute paths and diagnostic path/include chain/line/column/span never enter semantic hashes. Preserve exact registry, package/compiler state, input nesting, diagnostics, recovery/native/AOT/fixpoint/snapshot parity, and fail closed on missing, extra, reordered, digest-mismatched, or unused manifest entries. Add direct-versus-framed regressions for every proven runtime and compiler divergence plus arbitrary immediate parse-name, nested/transitive/repeated loaders, and mutation after freeze. Files: authenticated frozen-provider and loader/compiler integration, bootstrap mirror, focused provider tests; no second frame stack and no source-map, scanner, or diff edits. Verify: native/recovery/fixpoint/AOT frame suites, hb-build focused gate, full cold gate. Dependencies: the existing safe-filesystem milestone and `habu-authenticated-source-frame-7c4e1a90`; checker loader-preflight lane landed; no parallel compiler edits. Ownership: authenticated frozen loader provider and compiler integration only. Claim: unassigned.

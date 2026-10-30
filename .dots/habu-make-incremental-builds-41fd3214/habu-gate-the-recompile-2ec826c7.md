---
title: Gate the recompile count after small edits
status: active
priority: 1
issue-type: task
created-at: "2026-10-22T12:17:10.199700+03:00"
blocks:
  - habu-build-the-engine-0f80d46e
---

Problem: no gate row measures recompilation after a small edit. test/compiler/native-build-edit-e2e.f is in no gate (docs/gate.md:28-33 optional), and its COUNTS (:437-450) asserts only hits>0 and compiled>0, which every measured regression passes. DIAG-CASE in test/native-file-auto-e2e.f:369-370 checks refusals are absent with HABU_NATIVE_CACHE_DIAG off, so it can never fail. Acceptance: one gate row (SUITE native-build-edit in test/gate-stdlib-cases.f, extending test/compiler/native-build-edit-e2e.f) with a persistent private cache, counting files: the build report prints `files reused N compiled M` and the compiled paths. Through hb-build on a realistic multi-file fixture (several packages per file, a type declaration, TRUSTED:, a does> definer) and through hb-build --engine: no edit -> artifact hit; comment edit in src/lib/memory.f -> exactly 1 file compiled; body-only edit -> 1; interface edits (rename a public MEMORY word, change a constant, add immediate; cases from ~/.cache/tmp/carl-file/native-file-keys-e2e.f) -> memory.f plus exactly the files that consumed the changed fact. Each product runs and equals a fresh-cache build; image, map, files.txt and the warm wall time are kept as artifacts. Delete COUNTS and the vacuous DIAG-CASE checks. Files: test/compiler/native-build-edit-e2e.f, test/gate-*.f, docs/gate.md. Depends: the engine-through-file-path dot; habu-delete-the-arm64-b319376e for the hb-build rows (OBJECT-OPEN refuses every file whose tier is not 1, so AOT and REPL builds reuse nothing until the tier goes). Claim: agent=carl worker workspace=.jj-ws.noindex/carl-gate (lane gate-A, the engine row; brief carl-dots/.out/carl-file/brief-gate.md; the hb-build fixture rows are a second brief).

Ruling (carl, 2026-10-28, Joel: move forward on tier 1, cut over later): the gate row runs every hb-build row at tier 1 (a tier-1 caller; HBB-TIER takes the caller's tier) and the engine row as the engine build runs today. It does not wait for the tier 0 lanes; at the tier 0 cutover (habu-publish-and-shadow-8ea8909d, habu-delete-the-arm64-b319376e) this row is part of their acceptance and must stay green at tier 0.

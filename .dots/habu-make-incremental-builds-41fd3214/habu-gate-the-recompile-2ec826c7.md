---
title: Gate the recompile count after small edits
status: open
priority: 1
issue-type: task
created-at: "2026-10-22T12:17:10.199700+03:00"
---

Problem: no gate row measures recompilation after a small edit. test/compiler/native-build-edit-e2e.f is in no gate (docs/gate.md:28-33 optional), and its COUNTS (:437-450) asserts only hits>0 and compiled>0, which every measured regression passes. DIAG-CASE in test/native-file-auto-e2e.f:369-370 checks refusals are absent with HABU_NATIVE_CACHE_DIAG off, so it can never fail. Acceptance: one gate row (SUITE native-build-edit in test/gate-stdlib-cases.f, extending test/compiler/native-build-edit-e2e.f) with a persistent private cache, counting files: the build report prints `files reused N compiled M` and the compiled paths. Through hb-build on a realistic multi-file fixture (several packages per file, a type declaration, TRUSTED:, a does> definer) and through hb-build --engine: no edit -> artifact hit; comment edit in src/lib/memory.f -> exactly 1 file compiled; body-only edit -> 1; interface edits (rename a public MEMORY word, change a constant, add immediate; cases from ~/.cache/tmp/carl-file/native-file-keys-e2e.f) -> memory.f plus exactly the files that consumed the changed fact. Each product runs and equals a fresh-cache build; image, map and files.txt are kept as artifacts; warm wall time is recorded in docs/compiler-measurements.md beside §11. Delete COUNTS and the vacuous DIAG-CASE checks. Files: test/compiler/native-build-edit-e2e.f, test/gate-*.f, docs/gate.md. Depends: the engine-through-file-path dot. Claim: unassigned.

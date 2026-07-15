---
title: Compile authenticated source frames
status: open
priority: 2
issue-type: task
created-at: "2026-07-16T00:44:24.858224+02:00"
blocks:
  - habu-checker-reject-compile-c8805039
---

Problem: destruction review proved flat textual source concatenation cannot preserve Forth source-frame EOF semantics. Child frames ending in line or parenthesis comments, parse-name, char, tick, variable, constant, package, export, undefine, custom parsing immediates, and pending compiler parsers consume parent bytes or change failure classes; ordinary separator bytes cannot represent EOF because parsers skip or consume them. Implement a canonical authenticated frozen-source provider and compiler input-frame stack: hb-build supplies the frozen entry and dependency byte table plus content identities; normal include/included/require/required/provided resolve only through that table; every parser observes the real current frame limit; EOF returns to the suspended parent input state; no child can open mutable filesystem content after freeze. Preserve exact registry, package/compiler state, input nesting, diagnostics, recovery/native/AOT/fixpoint/snapshot parity, and fail closed on missing, extra, reordered, digest-mismatched, or unused manifest entries. Add direct-versus-framed regressions for every proven runtime and compiler divergence plus arbitrary immediate parse-name, nested/transitive/repeated loaders, and mutation after freeze. Files: compiler/evaluator input-frame owner, loader frozen-provider boundary, bootstrap mirror, focused frame tests; no source-map, scanner, or diff edits. Verify: native/recovery/fixpoint/AOT frame suites, hb-build focused gate, full cold gate. Depends: checker loader-preflight lane landed; no parallel compiler edits. Ownership: compiler input frames and authenticated frozen loader provider only. Claim: unassigned.

---
title: "Build: exact modular AOT source"
status: open
priority: 1
issue-type: task
created-at: "\"2026-07-15T14:23:10.016110+02:00\""
blocks:
  - habu-m2-safe-filesystem-6d289b3d
---

Full context: hb-build hashes the full require/include dependency closure but passes only the entry file to the native AOT maker, so any modular entry reaches maker with an undefined require/include word and exits 70. Implement one checked source-composition owner that materializes the exact program seen by normal Habu loading before AOT: recursively expand literal include/included at the original loader position with full multiplicity; expand require/required once according to the exact-string registry; honor provided without loading; preserve loader order, package-private scope, compiler state, and definitions around colon/immediate forms. Do not concatenate a deduplicated dependency list ahead of the entry because that changes package and compiler scope. Reject dynamic, shadowed, residual runtime, missing, cyclic, malformed, NUL-path, or capacity-exceeding composition fail-closed with the original loader site and include chain. Freeze every path and content byte once before cache lookup so the composer, diagnostics, cache key, checker, and maker consume one authenticated artifact rather than reopening mutable inputs. Preserve original path, line, column, and byte attribution through a checked source map for text and JSON diagnostics. Key the composer version, ordered loader plan and multiplicity, registry transitions, paths, content and map digests, plus the existing target/compiler/checker identities. Reuse the canonical source lexer; do not add a second parser. Compile modular top-level, transitive, duplicate-require, repeated-include, provided-before-require, shared-package-private, runtime-loader rejection, path-with-spaces/quotes, hostile comments/strings/CRLF/no-final-LF/empty/NUL, boundary capacity, dependency-edit, bad-dependency, cycle, and input-swap fixtures through real hb-build. Prove direct loading and built executable behavior match, cache hits and misses are truthful, single-file AOT remains unchanged, and the real bulk-diff scanner builds and validates its output. Add bootstrap/native parity plus docs/manifests. This blocks habu-tools-bulk-diff-f36d0508 and habu-tools-frame-diff-e98f8a6a. No scanner or diff-parser edits.

Current correction: destruction review rejected every flat textual composer and
source-map remapper because no separator preserves child-frame EOF semantics.
The only accepted implementation is the authenticated frozen input-frame provider
owned by habu-compile-authenticated-src-05e058a2; delete the stale composer branch.
Milestone routing: M3 only; start from green M2 master.
Claim: unassigned (stale claim stripped 2026-08-04: the named workspace no longer exists on disk or in `jj workspace list`). The lane workspace .jj-ws/habu-build-exact-modular-44f4c2dc is gone, so no in-lane evidence survives.

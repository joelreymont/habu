---
title: "Tools: frame diff path identities"
status: open
priority: 1
issue-type: task
created-at: "2026-07-15T09:16:01.381637+02:00"
blocks:
  - habu-m4-bulk-side-154a1139
---

Full context: tools/lint/diff.f consumes line-oriented jj diff --git, which cannot represent arbitrary POSIX path bytes or distinguish a valid rename-only EOF from truncation. Define one checked framed byte-stream artifact with declared raw-section lengths, form/body-present tags, length-prefixed old/new identities, exact section count, and content digest. Bind every raw section and HABUSIDE content row exactly once by ordered metadata/path/kind identity, preserve CR/LF bytes, reject form/count/order/digest/extra/missing/duplicate mismatch, migrate typed-local and kernel-perf producers/consumers, and hard-remove the line-only API. Production diff-capture must install and invoke the M4 bulk provider once; the default throwing provider and per-row test provider are not acceptable production paths. Publish only through M2 alias-safe atomic replace. Add real jj fixtures for pure/modified rename/copy, binary, duplicate rows, and LF/CR/space/tab/quote/backslash/ b/ / and / paths. No host parser, compatibility alias, or per-row process fallback. Milestone routing: M5 only. Claim: unassigned (stale claim stripped 2026-08-04: the named workspace no longer exists on disk or in `jj workspace list`).

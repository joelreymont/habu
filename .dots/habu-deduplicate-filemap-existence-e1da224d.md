---
title: Deduplicate filemap existence check
status: open
priority: 2
issue-type: task
created-at: "2026-07-27T09:38:47.215580+02:00"
---

tools/filemap-lint.f keeps a private FM-EXISTS? ( ptr u8 n -- bool ) that is a near-duplicate of EXISTS? in lib/fs.f:180 - both are path-existence checks, and the only reason the filemap copy keeps its prefix is that it duplicates the library word instead of calling it (it also differs by using LINT-PATHZ PATHBUF + access directly). Owned result: delete FM-EXISTS? and call the lib/fs.f word, or prove a real behavioral difference (symlink handling, error propagation) and name the word for that difference. This changes behavior surface, so it is its own leaf, not part of the packaging commit. Owner: package FILEMAP-LINT (landed in the vecmem lane wall commit). Acceptance: filemap-lint production gate green with identical path/finding counts on the same tree; fixture suite green; a regression proving the chosen semantics on a dangling-symlink fixture; both diff lints.

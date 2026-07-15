---
title: "Tools: frame diff path identities"
status: open
priority: 2
issue-type: task
created-at: 2026-07-15T09:16:01.381637+02:00
---

Full context: tools/lint/diff.f currently consumes line-oriented `jj diff --git`. jj 0.37 emits filename bytes raw; LF splits path records and CR can be mistaken for a line ending, so arbitrary POSIX paths cannot be represented soundly by `LINE` alone. After habu-lint-diff-share-486c2d86, define a checked Habu byte-stream input plus ordered length-prefixed identity manifest `(old-present, old-bytes, new-present, new-bytes)` for every diff section; NUL framing is also valid because filenames cannot contain NUL. Bind each section to exactly one manifest entry, validate raw head/text/rename/copy/binary metadata when representable, preserve CR/LF bytes, reject count/order mismatch and extra entries, migrate typed-local and kernel-perf CLI artifact producers/consumers, add real jj fixtures for LF/CR/space/tab/quote/backslash/` b/`/` and ` paths, document the artifact contract, update FILEMAP, and keep the old line-only API fail-closed for unrepresentable names until hard-removed in the same change. No host-language parser or compatibility alias.

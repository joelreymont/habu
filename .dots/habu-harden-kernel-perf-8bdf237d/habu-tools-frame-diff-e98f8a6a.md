---
title: "Tools: frame diff path identities"
status: active
priority: 2
issue-type: task
created-at: 2026-07-15T09:16:01.381637+02:00
blocks:
  - habu-lint-diff-share-486c2d86
---

Full context: tools/lint/diff.f currently consumes line-oriented `jj diff --git`. jj 0.37 emits filename bytes raw; LF splits path records and CR can be mistaken for a line ending, so arbitrary POSIX paths cannot be represented soundly by `LINE` alone. Raw EOF is also ambiguous: a valid pure rename/copy ends after its metadata pair, while a modified rename/copy has the identical prefix before its index/body, so truncation at that boundary cannot be detected from raw lines. After habu-lint-diff-share-486c2d86, define one checked Habu framed byte-stream artifact. Give every section a declared raw-byte length and form/body-present tag plus length-prefixed identities `(old-present, old-bytes, new-present, new-bytes)`; end the artifact with an exact section count and content digest so prefix truncation fails closed. Bind each raw section to exactly one frame, validate raw head/text/rename/copy/binary metadata when representable, preserve CR/LF bytes, reject length/form/count/order/digest mismatch and extra entries, migrate typed-local and kernel-perf artifact producers/consumers, add real jj fixtures for pure and modified rename/copy plus LF/CR/space/tab/quote/backslash/` b/`/` and ` and ` paths, document the artifact contract, update FILEMAP, and hard-remove the ambiguous line-only public API in the same change. No host-language parser or compatibility alias. Claim: agent=diff-frame workspace=.jj-ws/habu-tools-frame-diff-e98f8a6a.

---
title: "Diff: land side-content codec"
status: active
priority: 2
issue-type: task
created-at: "2026-07-17T01:49:29.344858+02:00"
---

Full context: safe change-file needs one deterministic binary artifact for ordered old/new side identities before the framed producer can cross-check bulk filesystem facts. Port only the reviewed HABUSIDE v1 codec and authenticated reader from the stale bulk workspace into the current verified base; preserve zero digest for absent/gitlink, SHA-256 for present file/symlink, strict repository paths, metadata binding, owned input, linear row traversal, and no embedded bodies. Files owned: tools/diff-side-content.f, tools/diff-side-content-read.f, tools/diff-side-content-test.f, docs/diff-side-content.md, focused gate wiring/FILEMAP rows only. Do not edit scanner, capture, compiler, filesystem, or shared diff metadata. Acceptance: focused codec/reader roundtrip; mutation/truncation/digest/path/state/large-linear negatives; typed-local, host, filemap gates. Integration owner will cross-check row identities and empty add/remove semantics. Claim: agent=review_fs_nofollow workspace=.jj-ws/habu-diff-land-side-98dd8f40.

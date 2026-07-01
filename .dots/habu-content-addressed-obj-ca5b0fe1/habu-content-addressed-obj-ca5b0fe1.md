---
title: Content-addressed object/linker build cache
status: open
priority: 2
issue-type: task
created-at: "2026-06-30T21:16:52.621274+02:00"
---

RCA: current Habu setup recompiles source files into a live dictionary/warm snapshot; content-hash caches final binaries/images only. Implement a real Habu object/linker layer: per-source object contains code bytes, data/literal init records, exported/imported symbols, relocations, package/require metadata, checker effect/no-return/type records; key by source hash + target/checker/compiler ABI; linker merges objects into candidate/warm/test images without recompiling unchanged source. This complements resident test suites; it does not replace subprocess tests for true CLI boundaries.

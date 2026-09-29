---
title: Own native build source bytes
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T14:29:49.473761+02:00"
blocks:
  - habu-define-exact-native-a4cb5657
---

At one build-owned boundary, collect canonicalization outcomes (including missing primary candidates) and each resolved source path/root and byte span once. Discovery scans those bytes, the key hashes them, and normal included/required load frames evaluate them for both retained host and freshly loaded target; unknown inputs refuse. Each loader keeps its own require registry and source frame behavior. Bind the target's fresh provider after its include.f load and restore normal callbacks before capture. Relevant files: src/core/include.f, tools/source-discovery.f, tools/event-closure-lib.f and native driver. Write checked E2E before code: collect a fallback dependency, replace it and create the missing primary, then retained and target loaders execute the original fallback bytes through nested included/repeated required. No metadata heuristic, duplicate byte/hash check, replay or generic sealing. This is prerequisite to keyed artifact publication.

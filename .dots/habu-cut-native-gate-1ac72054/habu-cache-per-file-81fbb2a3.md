---
title: Cache per-file content digests for warm keys
status: closed
priority: 2
issue-type: task
created-at: "\"2026-06-29T07:11:35.406913+02:00\""
closed-at: "2026-06-29T08:33:15.146505+02:00"
close-reason: "Implemented metadata-keyed per-file digest cache in lib/content-key.f with FILE-META support, invalidation tests, manifest/docs rows, and full native gate proof: 28.339s internal / 31.69s wall hot-cache."
---

Problem: hot full gate still spends ~10s per tools/checker warm cache-hit child because SUITE-WARM-KEY!/GE-WARM-KEY! rehash every baked source file each run. Rejected in-process validation: 46577ms internal / 50.54s wall because hashing serialized before real phases. Fix: add a Habu-native per-file digest cache keyed by path + stat metadata (size + mtime/ctime as available) and consumed by lib/content-key.f CK-FILE+ without weakening correctness; invalidate on changed metadata and fall back to SHA256-FILE. Files: lib/fs.f stat metadata API, lib/content-key.f, content-key tests, warm gate key callers. Acceptance: focused tools/checker warm cache-hit slices materially below 10s, hot full gate below current 40935ms without stale reuse; invalidation test changes a baked source and forces miss.

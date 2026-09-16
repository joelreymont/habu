---
title: Ship no dictionary records for private words
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-15T18:52:59.708169+03:00\""
---

Problem: the baked engine carries 15,854 records (48 B each, 0.76 MB) plus name pools for 34,000 definitions across 66 prefix files; 4,939 sit in the global wordlist. Private helpers need code, not names: a user cannot reach them, and the seed binds calls by record (see the sites dot). Acceptance: records for private words are dropped at capture (or retired to a build-side map kept for tools), the shipped dictionary holds the public surface and engine-internal words the interpreter must find; xref/debugger tools read the build-side map when they need private names; test/run.f green; record count and image size reported before/after. Files: src/habu/aot-capture.f, src/habu/aot-closure.f, src/habu/xref.f, tools/imgdump.f, docs. Verify: ndict@ on the shipped engine; bin/hb --load test/run.f. Depends: habu-bind-baked-call sites dot. Ownership: AOT capture. Claim: agent=hazel-private-words workspace=.jj-ws/hazel-private-words.
Parked 2026-09-16 17:12 (Opus session limit, resets 20:30): workspace .jj-ws/hazel-private-words, byte budget landed at 39ba124c; @ f2147916 holds uncommitted WIP on site binding (habu-bind-baked-call-e4d5b58f): aot-decl.f, habu2.f, tools/engine-size.f edited, emitter side unfinished. Resume by a new Opus worker in the same workspace with the decisions in hazel message of 16:55 (keep-set, sidecar, sealed-surface assumption).

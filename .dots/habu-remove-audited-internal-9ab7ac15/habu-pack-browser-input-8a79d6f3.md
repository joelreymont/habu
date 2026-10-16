---
title: Pack browser input allocations into chunks
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.936420+03:00"
---

Module.place grows a Wasm page for every payload; actual1000one-byte calls add62.5MiB capacity. Retain host-owned chunk cursor/limit and pack inputs, allocating another chunk when needed. Preserve retained bytes and module/host allocation separation even when module grows memory. Existing browser storage/host E2E and capacity reproduction. This narrows existing browser94c46576; full reclamation API is separate.

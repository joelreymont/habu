---
title: Pack browser input allocations into chunks
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.936420+03:00"
closed-at: "2026-10-16T11:43:44.212297+03:00"
close-reason: Packed host inputs into owned Wasm pages, preserving retained spans across module growth. Storage E2E retains distinct batches and the initial payload; echo, navigation, request and HTTP E2Es pass. Adversarial review and focused follow-up clear. Actual repeated-input test grows five pages for module storage instead of one page per input.
---

Module.place grows a Wasm page for every payload; actual1000one-byte calls add62.5MiB capacity. Retain host-owned chunk cursor/limit and pack inputs, allocating another chunk when needed. Preserve retained bytes and module/host allocation separation even when module grows memory. Existing browser storage/host E2E and capacity reproduction. This narrows existing browser94c46576; full reclamation API is separate.

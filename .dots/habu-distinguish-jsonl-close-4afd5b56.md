---
title: Distinguish JSONL close failures
status: active
priority: 1
issue-type: task
created-at: "\"2026-10-14T23:29:45.524776+03:00\""
---

Otto source-confirmed src/lib/jsonl.f at3164d371 READ-BLOCK EOF closes its read-only fd but throws E-FS-IO on close failure, the same code as failed read. JSONL:CLOSE has the same classification. Dots maps that code to read I/O failure; a close-only failure after every task was imported can skip its archive step. Actual close failure not reproduced on raw svc host; do not invent a demonstrated runtime crash or add syscall-interposition framework. Provide a distinct documented close error at the JSONL owner boundary, preserve actual failed-read and callback errors through cleanup, release fd/storage exactly once, and keep successful EOF/framing unchanged. Prefer existing JSONL error reservation and unbaked package-local error over engine ABI change or ignored close failures. Inspect existing JSONL/tool streaming tests before code; use a real close-failure path only if one can be exposed with existing public APIs, otherwise report failure-injection boundary honestly and run relevant real file/framing tests. Independent Astra review, focused source/native consumer checks, coherent commit/release; Dots migrations stay with Otto.

---
title: Own explicit JSON writer state
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T16:07:53.254928+02:00"
blocks:
  - habu-add-generic-bounded-359c0944
---

Why: JSON-WRITE stores its buffer, length, capacity, and number scratch in process globals, so nested or interleaved rendering corrupts output. Result: package JSON-WRITE publishes one linear writer plus STORAGE-BYTES, INIT, CLEAR, COPY, and CLOSE. INIT consumes caller-owned aligned state and bounded scratch and returns initialized(writer) or refused(state,scratch,error); the writer owns both spans until close. A private append core records exact required length after scratch exhaustion. CLEAR consumes and returns an empty writer. COPY ( JSON-WRITE:writer ptr u8 CAD-NUM:byte-len -- COPY:result<JSON-WRITE:writer> ) preflights the complete document and destination before any write. CLOSE consumes writer once and returns closed(state,scratch) with the exact original owners. This child leaves the existing singleton code untouched only so the unpublished cutover branch remains green; it adds no forwarding word or adapter, and the integration leaf deletes that code before master publication. Owner: writer representation, lifecycle, private append core, and copy only. Dependency: canonical bounded-copy result. Production red: no caller-owned JSON state exists. Acceptance: two states initialize, clear, copy empty output, close to their exact spans, refuse exact one-short state, scratch, and destination spans without changing sentinels, return exact requirements, and reject drop, duplicate, double close, or reuse through checked negatives. Forbidden: value emitter, structure emitter, singleton modification, old-name alias, adapter, raw span, allocation, second copy result, version, compatibility path, metric, or lint. Smallest owning check: the lifecycle slice of lib/json-write-test.f. Claim: unassigned.

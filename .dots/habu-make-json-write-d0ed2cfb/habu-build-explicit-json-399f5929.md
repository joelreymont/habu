---
title: Build explicit JSON writer core
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T16:07:53.254928+02:00"
blocks:
  - habu-add-generic-bounded-359c0944
---

Why: JSON-WRITE stores its buffer, length, capacity, and number scratch in process globals, so nested or interleaved rendering corrupts output. Exact interface: package JSON-WRITE publishes linear JSON-WRITE:writer; STORAGE-BYTES reports fixed aligned state bytes; INIT(ptr a, state-cap, ptr u8 scratch, len scratch-cap) validates exclusive caller-owned state and scratch and returns writer; RESET and every existing value, key, delimiter, and field emitter consume and return writer; COPY(writer, ptr u8 dst, len dst-cap) returns COPY:result<JSON-WRITE:writer>; CLOSE consumes writer. The state tracks exact required length after scratch overflow, writes only inside scratch, and COPY preflights both complete scratch and destination capacity before any destination byte changes. A required result preserves the writer and exact len so the caller can close, allocate larger scratch/output, and rerender. Null or misaligned state and null positive-capacity buffers reject with named JSON-WRITE errors. No public raw span exists. Owned result: explicit core and focused tests only; old singleton names are removed in this feature stack, so this commit is staged with consumer migrations and never lands alone. Acceptance: two writers interleave byte-exactly; nested render does not disturb either; too-small scratch and too-small destination both return exact required len and preserve a sentinel-filled destination byte-for-byte; success copies the whole document; checked negatives reject drop, duplicate, double-close, and using one writer state twice. Smallest check: bin/hb --load lib/json-write-test.f. Depends: habu-add-generic-bounded-359c0944. Ownership: lib/json-write.f, lib/json-write-test.f, FILEMAP.md. Claim: unassigned.

---
title: Implement close for streams
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:39:59.399522+02:00"
---

src/runtime/primitives/io.zig: Add close_stream(stream, abort). Close file handle, mark stream closed. Dependencies: habu-implement-open-for-ab213ded. Verify: (close stream).

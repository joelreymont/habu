---
title: Index source map lines
status: closed
priority: 2
issue-type: task
created-at: "2026-07-15T23:50:06.099263+02:00"
closed-at: "2026-07-21T22:08:46.753841+02:00"
close-reason: Superseded by direct cursor and origin indexing on authenticated source frames; the flat SOURCE-MAP index will not exist.
---

Full context: SOURCE-MAP line-column lookup rescans the complete composed source for every diagnostic. Build a checked line-start vector once during authenticated OPEN and resolve line/column with indexed bounds plus direct offset; EOF cursor is explicit. Acceptance: exact parity for LF, CRLF, no-final-LF and EOF; a many-diagnostic 1 MiB regression is linear in source plus diagnostics and materially faster than baseline; no allocation per lookup. Files: tools/source-map.f/test and a focused benchmark fixture.

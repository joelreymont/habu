---
title: Stream content keys through SHA contexts
status: active
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.888097+03:00"
---

content-key.f retains up to four256KiB complete preimages only FINAL reads; SHA256-BEGIN/FEED/END already exists with784byte contexts. Preserve byte-exact tag/length encoding and independent nested fold ownership, stream fields directly and delete preimage capacity/pointer bookkeeping and obsolete why-threw telemetry. Existing content-key golden/nested-fold and real cache-key consumers, no format change; independent review.

---
title: Stream content keys through SHA contexts
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.888097+03:00"
closed-at: "2026-10-16T11:30:32.912322+03:00"
close-reason: Replaced retained preimages with one aligned SHA context per live fold, preserving exact key encoding and nested ownership. content-key, object-cache and event-closure tests plus changed-module verify-only checks passed; independent digest calculations matched overlapping folds and a327675-byte manifest. Independent Astra review found no material issue. Workspace retired.
---

content-key.f retains up to four256KiB complete preimages only FINAL reads; SHA256-BEGIN/FEED/END already exists with784byte contexts. Preserve byte-exact tag/length encoding and independent nested fold ownership, stream fields directly and delete preimage capacity/pointer bookkeeping and obsolete why-threw telemetry. Existing content-key golden/nested-fold and real cache-key consumers, no format change; independent review.

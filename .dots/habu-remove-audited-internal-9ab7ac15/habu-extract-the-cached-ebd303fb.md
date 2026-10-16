---
title: Extract the cached program payload directly
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.780207+03:00"
---

OBJIMG/object-image consumes one already-linked text program with one entry, yet src/lib/object-link.f simulates general multiobject symbol/relocation linking. Replace with bounded validated single-program extraction, preserving external object ABI/integrity and entry metadata; remove synthetic unused linker branches/tests. Coordinate hb-build-lib.f adapter with Carl; NUNIT-OBJECT is real and outside deletion. Existing cold/cache/relink executable-output tests.

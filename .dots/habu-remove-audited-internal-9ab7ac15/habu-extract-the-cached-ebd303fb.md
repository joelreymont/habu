---
title: Extract the cached program payload directly
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.780207+03:00"
closed-at: "2026-10-16T12:33:47.131343+03:00"
close-reason: Removed the generic linker and extract the single already-linked program payload directly. External object syntax, content-key, ABI and program-shape checks remain at load boundaries; fresh builder output is stored without serializing and validating itself. Independent Astra review clear. Object image, object resolve/cache, AOT cache and stripped-cache E2Es passed; integrated hb-build CLI suite passed with lint removal.
---

OBJIMG/object-image consumes one already-linked text program with one entry, yet src/lib/object-link.f simulates general multiobject symbol/relocation linking. Replace with bounded validated single-program extraction, preserving external object ABI/integrity and entry metadata; remove synthetic unused linker branches/tests. Coordinate hb-build-lib.f adapter with Carl; NUNIT-OBJECT is real and outside deletion. Existing cold/cache/relink executable-output tests.

---
title: Validate canonical source maps
status: closed
priority: 1
issue-type: task
created-at: "2026-07-15T23:48:26.543832+02:00"
closed-at: "2026-07-21T22:08:46.733329+02:00"
close-reason: Superseded by authenticated source frames. Flat composed source maps are being deleted by habu-remove-synthetic-compose-373b117a, so there is no canonical source-map format to validate.
---

NOT READY 2026-07-16: target files tools/source-map.f and
tools/source-compose-test.f do not exist on master (rg: no HABUMAP2 or
SOURCE-MAP definitions anywhere); they are created by the compose/cut lanes
named in blocks:. Same evidence class as habu-quote-diagnostic-paths-5e982e5e
(diagquote lane BLOCKED report, 2026-07-16).

Full context: SOURCE-MAP accepts producer-impossible HABUMAP2: entry not file 0, reversed/cyclic chains, zero line/column, unchecked src+len overflow, and hand fixtures that violate SOURCE-COMPOSE order. Enforce exact producer grammar: entry id 0; root-first acyclic chains with existing prefixes and no repeated file; row file equals chain tail; positive line/column; checked output/origin arithmetic; canonical file/chain/row order; reject empty/unreferenced/impossible tables unless the producer can emit them. Replace hand authority with SOURCE-COMPOSE -> SOURCE-MAP authenticated roundtrip. Acceptance: one negative per invariant, empty-file behavior pinned from real producer, corrupted authenticated map rejects, valid nested/repeated loader maps open. Files: tools/source-map.f/test and tools/source-compose-test.f.

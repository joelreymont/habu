---
title: "Measure SOURCE-FUNCTION's scan at hb scale"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-06T18:48:23.853762+02:00"
closed-at: "2026-11-12T07:43:24.205375+02:00"
---

Problem: NSHADOW:SOURCE-FUNCTION (src/compiler/native/shadow.f, public readers) answers a quotation's emission and function by scanning every emission; the L2c CODE writer calls it once per quotation reference it classifies, and link-band.f has the same scan, so a file's object costs O(quotations x emissions). Acceptance: the share of an hb build's time spent in SOURCE-FUNCTION, measured with the L2c writer in place; if it is out of proportion, an index from function start to (emission, function) built once per build replaces the scan in both callers. Files: src/compiler/native/shadow.f, src/habu/linker.f, src/habu/link-band.f. Verify: tools/tier0-profile.f or the sampling profiler on an hb build; test/file-objects.f, test/linker.f. Depends: habu-file-objects-l2c-0d3765ca. Ownership: shadow readers. Claim: unassigned.

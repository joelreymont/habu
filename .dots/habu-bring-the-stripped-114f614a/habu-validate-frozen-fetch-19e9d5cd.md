---
title: Validate frozen fetch descriptor structure only once
status: open
priority: 1
issue-type: task
created-at: "2026-09-22T20:46:02.580160+03:00"
---

Problem: src/compiler/native/fetch.f KEEP validates each fetch descriptor with NFETCH-CHECK:SHAPE before interning its bytes in NSTR. src/compiler/native/elaborate.f VALIDATE-FETCH nevertheless emits NFETCH-CHECK:CHECK for every typed fetch, and fetch-check.f CHECK walks the same immutable descriptor through SHAPE again before checking the current value. That repeats structural validation at runtime; changing value tags still need their checks. Source verified on hazel/integration 1ba6e264. In the private compacted engine 3c7570bd6cad2943fc1fd3d48112323d9ff02a4119d23198ceb462de85a618ee, SHAPE alone is 596 bytes plus its validation callees. Acceptance: establish the immutable descriptor boundary; compiled fetches validate structure once at construction and only value tags at runtime, without weakening the public CHECK malformed-descriptor refusal or invalid-tag rejection (including a fetched value immediately dropped). Preserve nested/parametric/unaligned descriptor fixtures, run the owning native-fetch and stripped rows, compare code bytes and paired timings, then generation proof and the full gate. No release-mode flag or unchecked arbitrary-pointer entry. Files: src/compiler/native/fetch.f, fetch-check.f, elaborate.f and their existing fixtures. Ownership: Hazel compiler lane; Alder supplies the reduction. This is separate from compile-time scalar type checking and the cold-throw layout dot.

Measured on the pinned 1ba6e264 engine, SHA256
5db92ecab9d53b952b10e3191ee987685dd3db950413701c9c1d81af636bf061:
one million calls per batch, one warm-up then five alternating batches. A
single enum descriptor costs 38.47 ns/call for SHAPE alone, versus 67.86 ns
for full CHECK. The existing fixture's three-level nested descriptor costs
173.93 ns for SHAPE versus 305.43 ns for full CHECK. Both use valid values;
the nested case visits every active tag. Thus structural validation alone
costs about 57% of these full-check loops. This is a local microbenchmark,
not an application speedup claim. Probe and raw timings are
/tmp/alder-fetch-cost/probe.f and /tmp/alder-fetch-cost/timing.log; no compiler
source was changed.

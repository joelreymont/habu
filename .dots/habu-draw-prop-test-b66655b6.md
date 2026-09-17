---
title: "Draw property-test picks from the generator's high bits"
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T03:14:50.654529+03:00"
---

Problem: PROP:RND% in lib/property.f answers an LCG's low bits (RND bound mod); with a power-of-two bound the picks cycle in step with the draws, so a generator drawing 8 PROP:RND% per byte only ever produced four values and no control byte or quote in 2048 cases (found by the json-write lane, 2026-09-17; lib/xml-roundtrip-test.f already avoids it with 8 rshift bound mod, and lib/json-write-test.f now carries a local JWT-RND%). Every property test using PROP:RND% with a power-of-two bound explores a much narrower space than it appears to. Acceptance: PROP:RND% takes its pick from the high bits (or a mixed output such as an xorshift step over the LCG state), a test asserts that 4096 draws of 8 PROP:RND% cover all eight values and that consecutive draws are not a fixed cycle, the two local workarounds are deleted in favour of it, and the randomized suites that depend on PROP:RND% still pass at their case counts. Files: lib/property.f, lib/property-test.f, lib/xml-roundtrip-test.f, lib/json-write-test.f. Verify: those suites; test/run.f. Depends: habu-give-the-json-fd2ba9fc. Ownership: lib/property.f. Claim: unassigned.

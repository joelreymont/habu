---
title: An older hb builds 7205e0b6 into an engine without HBB-MAIN
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T12:08:00.053971+02:00"
---

Problem: an hb built before 7205e0b6 ("Carry hb-build's files in hb", which adds tools/hb-build-lib.f to tools/hb-build-core.f's baked list and exports HB-BUILD-CLI:HBB-MAIN) builds that tree without error, but the engine it makes fails on its own tools/hb-build.f:23 with undefined word 'HB-BUILD-CLI:HBB-MAIN': the engine skips the require of a file it does not contain. Seen on the spark: the 04:55 Linux hb needed 7205e0b6's parent built first. Acceptance: such a build either refuses, naming the stage it needs, or makes an engine that runs tools/hb-build.f; reproducer: build 7205e0b6 with an hb built from its parent, then run the result on tools/hb-build.f. Depends: none. Claim: unassigned.

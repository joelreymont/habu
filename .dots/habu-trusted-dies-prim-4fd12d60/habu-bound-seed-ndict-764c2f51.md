---
title: Bound seed-ndict! below and register the prefix checker test
status: closed
priority: 2
issue-type: task
created-at: "2026-09-11T18:15:57.452577+03:00"
closed-at: "2026-09-14T13:29:21.200239+03:00"
close-reason: Negative/out-of-range seed indices fail before pointer arithmetic; new native product passes build-rewind and engine suites plus prefix declarations. Registered checker owner-handoff fixture passes tier1. Earliest global marker policy unified; independent Astra review found no correctness defects.
---

Problem: src/habu/habu1.f:1304 seed-ndict! guards only n < ndict with a signed compare, so a negative boundary is accepted and sets NDICT to -1 (observed when a stale 'IMK-NDICT0 @ 1 - seed-ndict!' line met the now-zero IMK-NDICT0 cell). Also test/compiler/native-checker-prefix.f is in no SUITE of test/gate-stdlib-cases.f (passes by hand only); src/habu/snap-lib.f:204 still says util.f records the primitive record watermark; docs/bootstrap.md and LESSONS.md do not record that the prefix boundary is resolved from the running dictionary through CORE-PREFIX:FIRST-RECORD; CORE-PREFIX:FIRST-RECORD takes the highest wid-0 IMK-NDICT0 while hide.f BFR-MARKER-INDEX and bootstrap.sh take the lowest. Acceptance: seed-ndict! refuses n < 0 with the same exit class as n >= ndict and a bin/hb child test proves it; native-checker-prefix.f registered; the comment and the two docs corrected; the marker-selection policy made one (lowest or highest, stated) or the asymmetry documented as harmless with the reason. Files: src/habu/habu1.f, test/gate-stdlib-cases.f, src/habu/snap-lib.f, docs/bootstrap.md, LESSONS.md, src/core/prefix-boundary.f. Verify: engine-suite, native-prefix-declarations, native-checker-prefix. Depends: none. Ownership: rowan (engine file) or handed to hazel as a commit id. Claim: unassigned.

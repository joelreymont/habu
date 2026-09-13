---
title: "Forge the artifact reader's refusals"
status: open
priority: 2
issue-type: task
created-at: "2026-09-13T10:53:21.388233+03:00"
---

Problem: src/habu/aot-file.f lists the refusals its reader makes (a truncated payload, a doctored section table, a moved chain source, a wrong version, a bad producer key) and until 2026-09-13 its header claimed each had a forged case in test/aot-chain-capture-suite.f; none is forged, and test/aot-file-merge.f, named in aot-arm.f's header as MERGE's producer, does not exist, so AOT-FILE:MERGE has no check at all (measured by the S-PWID lane). Acceptance: a forged case per listed refusal, each doctoring a real artifact written by the round-trip fixture and asserting the named refusal and exit code in a child; a MERGE case that merges two small captures and reads the result back; the header's claims true. Files: test/aot-chain-capture-suite.f, test/aot-artifact-roundtrip.f, a new test/aot-file-merge.f, src/habu/aot-file.f (header). Verify: the suite, test/run.f. Depends: habu-retire-the-s-4fbc244f. Ownership: hazel. Claim: unassigned.

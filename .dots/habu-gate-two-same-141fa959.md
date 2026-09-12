---
title: Gate two same-host builds byte for byte
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T18:03:56.979500+03:00"
---

Problem: a611d84d made two same-host engine builds byte-identical and tools/two-generation-build.f asserts gen 5 = gen 4, but that assertion compares builds by different hosts and passed while the root tip 04d9b5ae already built two same-host engines differing by 3 bytes (measured by the combine lane on 2026-09-12 with its own files reverted): the allocator's ALLOC-NS-ACC stopwatch cell is the suspect, the same class as REG-PERSIST-DELTA. Acceptance: a gate that builds the tree twice from the same host and refuses a differing byte, naming the DATA offset and its owning record (the byte-reproducibility lane's probe method), cheap enough to run after every engine change (two cold builds, about 45 s), registered where the chain check is documented; the 3-byte drift removed at its source (habu-make-register-allocation follow-up). Files: tools/two-generation-build.f or a sibling tool, docs/bootstrap.md. Verify: the gate red on 04d9b5ae's tree and green after the fix. Depends: none. Ownership: hazel. Claim: unassigned.

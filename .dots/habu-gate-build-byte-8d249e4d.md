---
title: Gate build byte identity with a two-build compare
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T18:19:40.670747+03:00"
---

Problem: a build-time transient baked into the image (a clock, a pid, an unpinned address, an unordered walk) breaks byte identity between two same-host cold builds, and nothing in the merge path detects it: REG-PERSIST-DELTA (a611d84d), the register allocator's ALLOC-NS-ACC (rowan-alloc lane, 3 bytes at offset 5362463) and the combine lane's NPROF each landed or nearly landed this way; tools/two-generation-build.f names the class but takes 173 s and is in no suite. Acceptance: a gate tool that cold-builds twice from the same tree and refuses on any byte difference, printing the differing offsets and the DATA cell each falls in (name the owner through the snapshot table where possible); it runs in the merge gate for changes under src/ and tools/native-build.f, runtime about one minute; docs/bootstrap.md names it beside the generation chain check; a regression fixture with a deliberately baked clock value is refused. Files: tools/ (new gate), docs/bootstrap.md, the merge-gate definition. Verify: the gate on a clean tree passes; on the fixture it refuses by name. Depends: none. Ownership: hazel. Claim: unassigned.

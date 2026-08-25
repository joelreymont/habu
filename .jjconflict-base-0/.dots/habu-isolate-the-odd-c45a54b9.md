---
title: Isolate the odd attribute-window length arm
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T00:39:33.686600+02:00"
---

Full context: from agent irverify 2026-07-30, recorded as measurably uncovered. In the keyed-attribute layout (commit 81af2a24) an operation attribute entry is two pool cells, so a well-formed attribute window always has even cell length; the verifier arm refusing an odd length is defense-in-depth that the existing forge harness cannot isolate - deleting the arm leaves every test green (measured). Either extend the forge harness to corrupt a frozen row's window length directly through a legitimate table-level seam (the arena views are sealed; find or build the seam the storage gate's own forge rows use), or prove the arm unreachable through every checked path and record it as pinned defense-in-depth in formal/Common/Structure.v BINDING GAPS like the ARGS-CK this-block arm. Do not delete the arm and do not fake a fixture through a private cast.

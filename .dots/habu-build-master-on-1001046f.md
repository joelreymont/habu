---
title: Build master on an old host without NEMIT kind names
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T02:10:52.746238+02:00"
---

Problem: an engine older than master cannot build master's tree: src/habu/aot-shadow.f :233-234, :293, :411, :417; src/compiler/native/t0-emit.f:532; t0-x64.f:175, 179, 343, 348; t0-arm64.f:430, 441, 449, 454, 634, 639; src/compiler/native/link-wasm.f:361, 364, 429, 431 name NEMIT:ADDR-CODE and the other NEMIT kind words at compile time, so the old host (.jj-ws.noindex/jerry-b1-e1/bin/hb) stops at aot-shadow.f:293 with E-UNDEFINED NEMIT:ADDR-CODE, loaded through tools/native-build-deps.f:40 (observed in the nunit-kinds lane habu-own-the-unit-0965c0ab, which fixed the same defect in tools/native-unit-object.f by owning the format's constants and reading the window's numbering by name). Acceptance: the lead decides the rule for engine sources (the kinds are the tree's own constants, resolved from the tree's emission.f the host loads, or the host's names are read by name as the tool does) and a worker applies it; proof: the old host builds master's engine rc 0 and the result equals the public hb's build byte for byte. Files: the files above, src/compiler/native/emission.f. Verify: an old-host engine build and cmp; the full suite. Depends: none. Ownership: src/habu/aot-shadow.f, src/compiler/native/t0-*.f, link-wasm.f. Claim: unassigned.

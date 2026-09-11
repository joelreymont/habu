---
title: Make the engine image byte-reproducible
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T00:51:04.793412+03:00"
---

Problem: two tools/native-build.f builds by the SAME host differ in 3-4 bytes (2026-09-12, linux-aarch64), and the generation chain reaches its byte fixpoint only at generation 4 (B2 vs B3 1,129,827 differing bytes, B3 vs B4 1,015,834, B4 vs B5 3), measured by the fixpoint lane while closing habu-rebuild-an-engine-b2f3c39f; docs/bootstrap.md's byte-identical rebuild and the campaign's fixpoint acceptance both need the bytes to agree. Acceptance: name the 3-4 same-host bytes (a clock, a pid, an address of an unpinned mapping, or an unordered walk) and remove the source of each so two same-host builds are identical; then name why B2 and B3 differ by a megabyte when their shapes agree (host-dependent ordering or content the deficient B1 host lacks) and either fix it or record it as the documented chain length; tools/two-generation-build.f TG-SAME? grows a byte comparison of gen 3 against gen 2 (or the documented generation pair) once identity holds, with the shape line kept as the diagnostic. Files: tools/native-build.f, src/habu/aot-capture.f, src/habu/snap-lib.f, tools/two-generation-build.f, docs/bootstrap.md. Verify: cmp of two same-host products; the chain tool. Depends: none. Ownership: rowan (selfbuild). Claim: unassigned

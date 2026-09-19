---
title: Grow the discovery read buffer instead of capping it
status: active
priority: 2
issue-type: task
created-at: "2026-09-17T13:24:25.059847+03:00"
---

Problem: tools/source-discovery.f reads each file whole into one lazily allocated buffer capped at SD-SRC-CAP, raised $80000 -> $100000 in 7967a5b8 because src/core/checker.f is $A76BE bytes; the cap is a fixed number against a file that keeps growing, and lib/source.f already has the growing-buffer pattern (BUF-ENSURE / BUF-GROW). Acceptance: discovery adopts the growing buffer, SD-SRC-CAP is gone, a fixture walks a file larger than the old cap, and the discovery and closure tests pass. Files: tools/source-discovery.f, tools/source-discovery-test.f. Verify: the tests; test/whitebox-engine-key-test.f. Depends: none. Ownership: gate harness. Claim: alder.

The cached read buffer is now an owned `SPAN:span<u8>` sized from FILE-SIZE.
Growth installs the new mapping before freeing the old one and does not copy
scratch contents. Empty files still receive storage; later smaller reads reuse
it, and READ-ALL's returned length bounds the scan. A file that grows beyond
the available storage during reading still refuses instead of truncating.

Validation: source-discovery passes tiers 0 and 1. Its first fixture reads an
empty source, then a loader beyond 1 MiB, then appends another loader to force a
second growth; event paths, kinds, and exact offsets are checked. Existing
smaller fixtures follow. The old implementation throws E-FS-CAPACITY (-2106)
on the new large fixture. Standalone event-closure and whitebox-engine-key rows
pass, as does the whole four-file hb-build-fixtures row. Astra review clear.
Full gate and closure remain with Hazel; no shared engine writes.

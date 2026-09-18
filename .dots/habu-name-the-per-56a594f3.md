---
title: Name the per-definition literal-bytes limit
status: active
priority: 2
issue-type: task
created-at: "2026-09-17T23:47:54.226439+03:00"
---

Problem: a definition holding about 8 KiB of string literals (a case with nine 900-byte s" arms) refuses to load with status 71, the literal echoed and no error name, while eight such arms or fifty short ones load (aspen/Tender 2026-09-17; reproducer in ~/.cache/tender/habu-gaps/literal-bytes-per-definition.md). The limit is real but unnamed: nothing says which ceiling was hit, what it is, or how to split the definition. Acceptance: the ceiling identified (the literal pool per definition, the token buffer, or the compile buffer that overflows) and named by a diagnostic with the count, the ceiling and the definition's name in the style of lib/ffi-abi.f REPORT-FULL; the ceiling raised or made growable if it is arbitrary; a regression that compiles a definition at and just past the limit. Files: src/habu/ (the buffer that overflows), src/core/ (if the checker's literal store), test/. Verify: the regression; test/run.f. Depends: none. Ownership: engine compile buffers. Claim: agent=hazel-capacity workspace=.jj-ws/hazel-capacity.

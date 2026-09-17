---
title: Grow the discovery read buffer instead of capping it
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T13:24:25.059847+03:00"
---

Problem: tools/source-discovery.f reads each file whole into one lazily allocated buffer capped at SD-SRC-CAP, raised $80000 -> $100000 in 7967a5b8 because src/core/checker.f is $A76BE bytes; the cap is a fixed number against a file that keeps growing, and lib/source.f already has the growing-buffer pattern (BUF-ENSURE / BUF-GROW). Acceptance: discovery adopts the growing buffer, SD-SRC-CAP is gone, a fixture walks a file larger than the old cap, and the discovery and closure tests pass. Files: tools/source-discovery.f, tools/source-discovery-test.f. Verify: the tests; test/whitebox-engine-key-test.f. Depends: none. Ownership: gate harness. Claim: unassigned.

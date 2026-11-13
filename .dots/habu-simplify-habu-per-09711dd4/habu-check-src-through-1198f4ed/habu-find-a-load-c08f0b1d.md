---
title: Find a load required under using
status: open
priority: 2
issue-type: task
created-at: "2026-11-01T09:12:30.310430+02:00"
---

Problem: tools/source-discovery.f misses a load hidden behind a parses: word used bare under `using` (fixture usingreq, whose files are lost: the engine prints LOADED, discovery misses dep.f on master and on lane disc). A bare use resolves through the using imports' public words too, which the use lookup (SD-R-KEY-MATCH?, SD-R-FIND) does not read. Acceptance: a bare use under `using P` selects P's public rows as the engine resolves them, or discovery refuses; usingreq finds dep.f or is refused; a tools/source-discovery-test.f case (suite row source-discovery) failing before. Files: tools/source-discovery.f, tools/source-discovery-test.f. Verify: tools/source-discovery-test.f. Depends: lane disc landed (habu-key-discovery-records-b367e77e). Ownership: tools/source-discovery.f. Claim: unassigned.

A row reached through `using Q` misses the same way (fold 5 review fixture usingrow: Q public declares W parsing 1 with its row; `package P using Q W \ require dep.f ;using ;package`): engine LOADED, master and lane disc find entry.f only.

Census 2026-11-04: a disagreement between two implementations of one thing; it is closed by its parent removing the second implementation, not by patching that copy.

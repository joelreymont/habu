---
title: "Find the load behind a bare in-package parses: word"
status: open
priority: 2
issue-type: task
created-at: "2026-11-01T04:14:09.306392+02:00"
---

Problem: inside the package that declares a parsing word W with a parses: row, the bare spelling W does not select W's row in tools/source-discovery.f, so the operand is read as ordinary source and a load it hides is missed (fixtures /tmp/jerry-pkg2b-r2-tmp/probes/barein, barepart: the engine prints LOADED, discovery finds entry.f only). Missed on rnqmvxss too. Acceptance: a bare in-package (or in-part) spelling selects the row the engine resolves, or discovery refuses; barein and barepart find the load or are refused; an event-closure-test case failing before. Files: tools/source-discovery.f (SD-R-KEY-MATCH?, SD-R-AFFECTS?), tools/event-closure-test.f. Verify: tools/event-closure-test.f, tools/source-discovery-test.f; the fixtures. Depends: pkg2b landed (habu-read-a-pkg-272bc1d0). Ownership: tools/source-discovery.f. Claim: unassigned.

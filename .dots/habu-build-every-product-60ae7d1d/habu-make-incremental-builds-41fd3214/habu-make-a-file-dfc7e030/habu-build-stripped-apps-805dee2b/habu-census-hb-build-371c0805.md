---
title: "Census hb-build's stripped apps through LINKER:WRITE"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-06T01:54:35.510330+02:00"
closed-at: "2026-11-06T02:10:45.456081+02:00"
close-reason: "census done: 1/21 apps link+run through LINKER:WITH; 20 blocked on loading hb's startup files into the space (E-UNDEFINED require); findings in the parent dot and .out/carl-file/s7-census-apps.md, s7-features.md, s7-census-q4.md"
---

Problem: step 5a (parent dot) moves hb-build's stripped mode onto LINKER:WITH and LINKER:WRITE in hb's own process; which of the apps its suites build already link and run that way, and what fails first for the rest, is unmeasured. Acceptance: the report the brief asks for (per app: the test's checks, the new path's result, the first failing cause path:line, the old path's handling; hb-build's stripped-mode features with path:line; the suites' times). Files: probes under the workspace's .out/carl-s7/ only. Verify: briefs .jj-ws.noindex/carl-dots/.out/carl-file/brief-s7-census.md (apps, workspace carl-s7-census) and brief-s7-features.md (features, workspace carl-s7-feat); the suite times are the lead's background run. Depends: none. Ownership: carl. Claim: agent=carl, workspaces carl-s7-census, carl-s7-feat.

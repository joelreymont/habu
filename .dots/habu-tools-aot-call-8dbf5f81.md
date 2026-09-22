---
title: tools/aot-call-report.f includes its lib instead of requiring it
status: open
priority: 3
issue-type: task
created-at: "2026-09-22T08:00:53.011763+03:00"
---

Problem (found by the tools-requires lane): tools/aot-call-report.f:4 is 'include tools/aot-call-report-lib.f' while every other tools entry requires its lib; an image that also requires the lib (test/gate-aot-image.f does) would load it twice through this entry, and include leaves no registry record. Acceptance: the line is 'require tools/aot-call-report-lib.f'; 'bin/hb tools/aot-call-report.f <binary>' reports as before; test/run.f green. Files: tools/aot-call-report.f. Depends: none. Ownership: hazel (tools load path). Claim: unassigned.

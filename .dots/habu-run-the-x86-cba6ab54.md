---
title: Run the x86 peer images and fix the ones that crash
status: active
priority: 1
issue-type: task
created-at: "2026-11-25T16:05:33.397346+01:00"
---

Problem: 24 of the 222 x86 peer images end with another status than their test documents (writes through the region's executable view, fault texts that are now records, an index entry a retired word keeps, cells nothing fills), and the suites check only ELF headers. Acceptance: each image's status on x86 is what its test documents, and the suite runs the images on an x86 host (X64HARNESS:WRITE states the status an image exits with). Depends: habu-take-a-fault-e541c517. Claim: agent=worker workspace=.jj-ws.noindex/jerry-x-skel3.

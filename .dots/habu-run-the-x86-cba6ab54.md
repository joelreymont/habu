---
title: Run the x86 peer images and fix the ones that crash
status: active
priority: 1
issue-type: task
created-at: "2026-11-25T16:05:33.397346+01:00"
---

Problem: the ten hb-x64-skel images, kernel-control, -eval-closed and -eval-range exit 139 where their tests document 3, 102, 70, 0 and 89, and the suites check only ELF headers. Acceptance: each image's status on x86 is what its test documents, and the suite runs the images on an x86 host. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-x-skel (hunt).

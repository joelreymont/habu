---
title: "Merge image-bytes' length words into str words"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-22T16:47:39.688502+01:00"
closed-at: "2026-11-23T10:11:21.742421+01:00"
---

Problem: M-BYTES-LEN, M-NAME16-LEN and M-BE-BYTES-LEN (src/os/image-bytes.f) now duplicate M-BYTES, M-NAME16 and M-BE-BYTES. Acceptance: one word each, callers follow, suites pass. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-p2mby

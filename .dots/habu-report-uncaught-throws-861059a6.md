---
title: Report uncaught throws from x86-64 stripped images
status: open
priority: 3
issue-type: task
created-at: "2026-10-25T02:12:52.131517+01:00"
---

Problem: vlrlvnsq made arm64 stripped images report uncaught throw codes; x86-64 stripped images still exit without the report. Acceptance: an x86-64 stripped image that throws uncaught prints the same report as arm64; the uncaught-throw test runs on both targets. Verify: the stripped-image uncaught test on linux-x86-64. Host: Linux (out of scope until Linux work resumes).

---
title: Qualify x86 HTTP consumers past io_uring EPERM
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T10:48:16.862279+01:00"
---

Problem: the branch's executor refused io_uring_setup with EPERM, so HTTP consumers were never qualified on x86. Acceptance: an HTTP consumer runs on pulsar, or the EPERM is traced to its cause. Depends: none. Claim: unassigned.

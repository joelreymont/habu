---
title: Grow the Linux AIO ring with its records
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T22:55:00.021808+02:00"
---

Problem: src/lib/aio.f caps a Linux process at AIO-RECS-MAX ($1000) live records and its rings at AIO-CQ-ENTRIES, so a server such as WS (two records per socket plus its wake) gets E-AIO-FULL mid-serve once it holds about 2000 sockets. Approach (long-term rule, 2026-11-19): extend the ring doubling of dot habu-one-process-s-0377e7f0 past AIO-CQ-ENTRIES as the records grow; no in-place resize (IORING_REGISTER_RESIZE_RINGS needs SINGLE_ISSUER, one thread doing every submit and wait, and Linux 6.13, which Ubuntu 24.04 and Debian 12 lack); the 5.10 floor stays; no new public AIO word. Acceptance: a process holds records up to what its memory and RLIMIT_MEMLOCK allow, with no E-AIO-FULL from a record count; no completion is dropped; aio's tests cover a growth past today's cap. Depends: habu-one-process-s-0377e7f0. Claim: unassigned.

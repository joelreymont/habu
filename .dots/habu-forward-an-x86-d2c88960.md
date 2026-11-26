---
title: Forward an x86-64 profiler tick to its owner thread
status: active
priority: 2
issue-type: task
created-at: "2026-11-26T11:25:41.786916+01:00"
---

Problem: src/habu/prof-x64.f HANDLER, writes profiler state on whichever thread SIGALRM lands on; arm64 (src/habu/prof.f:39-48, :696-710) sends a delivery on any other thread to ARN-OWNER-TID and returns, so only the owner's handler writes state. On x86 a task thread's tick races the owner's QUIET, and samples count other threads' code. Acceptance: x86 forwards a non-owner delivery to ARN-OWNER-TID (tgkill) and writes nothing, as arm64 does; x86-64-kernel-prof passes on pulsar. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-xfwd.

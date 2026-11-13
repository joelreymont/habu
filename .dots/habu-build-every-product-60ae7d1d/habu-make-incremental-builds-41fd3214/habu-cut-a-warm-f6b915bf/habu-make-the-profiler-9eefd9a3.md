---
title: "Make the profiler's inclusive counts true"
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T04:37:15.158739+02:00"
---

Problem: tools/build-profile.f's inclusive counts are wrong both ways: src/habu/prof.f:169 counts every stack cell that holds a code address as a frame, stale ones included (LINKER:TOP-SCOPE?, one line that returns before LINK-PART, shows 17.7% inclusive with no self samples), and src/habu/prof-abi.f:82 stops after 64 cells within the 4 KiB block (HB-BUILD-CORE:ENGINE-BODY shows 20.4% of a build that spends about 61% of its wall time in it). Acceptance: the profiler walks the real call chain, so a word's inclusive count is the samples taken while it is on the stack, checked on a build whose phases were timed. Depends: none. Claim: unassigned.

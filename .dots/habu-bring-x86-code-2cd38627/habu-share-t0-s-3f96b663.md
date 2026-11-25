---
title: "Share T0's unit placement"
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T15:18:20.234520+01:00"
---

Problem: t0-arm64.f sizes frames, epilogues and branches at CLOSE; t0-x64.f patches fixed-size frames and rel32 branches in place, so x86 units carry 289 KB of frame adjustments and 80 KB of branch bytes arm64 does not. Acceptance: one placement part (t0-place.f) places arm64's units byte-identically, and x86's with no empty frame and rel8 branches where they reach. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-x-place

---
title: Run native-rstack-input at tier 1 on x86-64
status: open
priority: 2
issue-type: task
created-at: "2026-10-23T17:33:29.543120+03:00"
---

Problem: test/compiler/native-rstack-input.f runs ADMITTED-BOTH and REFUSED-AT at tier 0 unconditionally, but kernel-x64.f SET-TIER-BODY exits 70 ('set-tier: x86-64 runs tier 1 only'), so the suite likely fails on x86-64 (found by the g3a test audit lane; not run on x64).
Acceptance: the tier loops start at 1 on x86-64 as native-local-case.f does; the suite passes on arm64 and x86-64.
Verification: bin/hb --load test/compiler/native-rstack-input.f on an arm64 and an Intel host.

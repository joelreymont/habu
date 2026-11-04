---
title: Build the snapshot hole fixture on x86
status: open
priority: 2
issue-type: task
created-at: "2026-11-01T20:15:09.355646+02:00"
---

Problem: on x86 (master efef8267's engine and the t0x86 stack alike) building test/snapshot-writer-hole.f fails with 'hb: interpret stack underdepth: STR-MAX-I64$', so test/snapshot-writer.f F79 fails (expected rc 0, got 70; /tmp/jerry-big/x86base-suite.log:3299 and :3312 for F124). arm64 builds it. The fixture names src/lib/string.f:68's created table STR-MAX-I64$ at interpret level (snapshot-writer-hole.f:60-61 '0 STR-MAX-I64$ !') and in SEED-CELLS/SEED-NEXT (33-34). Root cause unknown: find whether the x86 engine's loader, the created word's effect or the interpret depth check is wrong, and fix that layer. Acceptance: the hole fixture builds on x86 and snapshot-writer's hole cases (F79-F94) pass there; arm64 unchanged. Files: to be found. Verify: on pulsar, bin/hb --load test/snapshot-writer.f; on the Mac, the same row. Depends: none. Ownership: the lane that takes it. Claim: unassigned.

Census 2026-11-04: an acceptance case of its parent; fix it in the code that stays (named above), never in the machinery the parent replaces.

---
title: "TFAM 2c: AOT protected-WID persistence + WIDN advance"
status: open
priority: 2
issue-type: task
created-at: "2026-07-03T23:36:48.919362+02:00"
---

PLAN.md item 2 (AOT half). Persist protected-WID registry through seed capture/restore with widened WID field (no u8 truncation, src/habu/aot-capture.f:133-160); restore WIDN above max restored WID before user wordlist/package allocation; reject sealed/generated WIDs in record registration, relocation lookup, bootrun; snap-rebase friend-only or protected-range checked. Fixtures: WIDs >255 round-trip. Gate 17b. Depends: TFAM 2b.

---
title: Allocate checker tables on first use
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:30.840099+03:00"
---

Problem: 59 static *-BOOT seed arrays hold about 644 KB of DATA (SYM-STR-BOOT alone is 393,216 B, checker.f:6976). Growth replaces them with an mmap, and from then on they are dead zeroes in every image. The TV pool already allocates on first use. Acceptance: every such table starts empty and allocates on first growth, the way the TV pool does; the *-BOOT arrays are deleted; image DATA drops by about 644 KB (state the measured size before and after); checker suites green. This removes unused zero-filled storage; it is not compression. Files: src/core/checker.f. Verify: rebuild bin/hb (checker is in the engine prefix); checker suites named by test/gate-stdlib-cases.f; bin/hb --load test/run.f (shared checker behavior). Depends: carl-final-core chain landed on master (it edits these files); start from master after it lands. Ownership: checker.f table storage. Claim: unassigned.

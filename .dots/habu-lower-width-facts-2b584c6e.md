---
title: Lower width facts in a tier-0 does> head
status: open
priority: 2
issue-type: task
created-at: "2026-10-23T18:41:43.046358+03:00"
---

Problem: at tier 0 a does> definer whose head carries width facts is refused rc 75 `does>-split cannot lower layout width facts`; tier 1 compiles it (2026-10-10 probe d5 printed 19 20 21 at tier 1). Cause recorded 2026-10-10 (carl), unverified since: the head and clause checks write one certificate buffer and pass 2 replays only the head's (src/habu/habu2.f C-CALL-CHECK-DOES, LOWER-TXN:FREEZE, EM-P2-TRIGGER). The original probes are gone: reproduce first, at tier 0 (`0 set-tier`) and tier 1 (`1 set-tier`) on ARM64. Tier 1 is the reference.
Acceptance: the definer compiles at tier 0 and prints 19 20 21 at both tiers. Verify: rebuild bin/hb per docs/gate.md; the reproducer as a native-suite row; `bin/hb --load test/run.f`; two-generation build. Files: src/habu/habu2.f, bootstrap/cg/forth.fs where the seed mirrors the path, the test row. Claim: unassigned.

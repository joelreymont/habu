---
title: Keep cells under catch when x86 constant fails
status: open
priority: 2
issue-type: task
created-at: "2026-11-01T20:15:09.366697+02:00"
---

Problem: 'check@ 0 set-check s" dup set-check drop constant K1" ' evaluate catch 0 set-check . . . . cr' prints 'E-UNDERFLOW: K1' then '70 384 70 70' on every x86 engine (master's and the t0x86 stack's), at tier 0 and tier 1: the failure overwrites cells below the catch depth. arm64 prints 'hb: interpret stack underdepth: K1' and keeps its cells. The Fable review of the t0x86 stack traced it to the Habu loop's constant definer, src/habu/definers.f DEF-CONSTANT's 'depth 0= if s" E-UNDERFLOW: " REFUSE then'. Probes: /tmp/jerry-fbrv-tmp/p7c.f (tier 0) and p7d.f (tier 1), on the Mac and on pulsar. Acceptance: x86 reports what arm64 reports and the cells under the catch survive; a rejected-program row through the real load path. Files: src/habu/definers.f and whatever the root cause names. Verify: both probes on both hosts, then the row. Depends: none. Ownership: the lane that takes it. Claim: unassigned.

Census 2026-11-04: an acceptance case of its parent; fix it in the code that stays (named above), never in the machinery the parent replaces.

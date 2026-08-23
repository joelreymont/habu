---
title: host-profile skill and gate.md disagree with the runner
status: active
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:26.028039+02:00"
---

Problem: skills/habu-host-profiles/SKILL.md:88 'TR-CAL-REF-SPARK-MS = 87' (the constant is CAL-REF-SPARK-MS, test/run-lib.f:76); :15-27 presents --budget-ms/--wall-budget-ms that test/run-lib.f:206 does not accept (0 source occurrences); docs/gate.md:201-205 lists three profiles, the runner has four (PROFILE-DGX-SPARK-10X2 run-lib.f:315); docs/gate.md:256-262 vs LESSONS.md:1163-1168 contradict on budgets; this host (Apple M2 Max under Linux, 12 CPUs) falls to linux-arm64-4x2 (4 slots) with no profile of its own. Acceptance: the skill's flag list regenerated from run-lib.f:206; gate.md has the Spark row and a row for this host; a new host profile for Apple-silicon Linux (detect via /proc/device-tree/compatible apple,*) with measured budgets; the budget lesson retired. Files: skills/habu-host-profiles/SKILL.md, docs/gate.md, test/run-lib.f, LESSONS.md. Verify: bin/hb --load test/run.f -- --timings on this host picks the new profile. Depends: engine on this host. Ownership: gate runner. Claim: agent=host-timing workspace=.jj-ws/habu-runner-budgets-uncalibrated-cb11c328

---
title: Index checker order keys once at handover
status: open
priority: 3
issue-type: task
created-at: "2026-10-22T14:59:32.126208+03:00"
---

Problem: the checker handover (tools/native-build-core.f ~:535, CHECKER-REG:ORDER-* words) recomputes the order key per record: 0.35 s of every warm engine build, 214 exclusive samples. Evidence: `hb-build --engine` builds timed with `/usr/bin/time -l`. Acceptance: order keys computed once per handover and looked up per record; handover time measured before/after; behaviour unchanged. Files: tools/native-build-core.f and the CHECKER-REG order words it calls. Verify: warm engine build timing, test/native-file-auto-e2e.f. Depends: none. Claim: unassigned.

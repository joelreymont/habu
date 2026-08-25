---
title: evidence and schedule keys hardcode sm_87
status: open
priority: 1
issue-type: task
created-at: "2026-08-22T22:38:25.978411+02:00"
---

Problem: maki/cad.f:1241,1251,1409,1410 and maki/lower/model-device.f:147 use TARGET:SM87 for SK-KEY$, REPORT:CACHE!, EVID-PUT-G, SK-PUT-DURABLE while maki/eval/active-target.f:40-43 already resolves the real device (GB10 = TARGET:SM121A, anything else throws); competitive-evidence-store.f:242 admits 'the only corpus target (orin-nx sm_87)'. On the GB10 every replayed schedule and evidence row is keyed to a different GPU than the one that produced it. Acceptance: ATGT:ID (or an explicit host-only 'no target') threaded into every key; the constant removed; a test on a fake target shows the key follow it. Files: maki/cad.f, maki/lower/model-device.f, maki/eval/active-target.f. Verify: maki/test.f. Depends: none. Ownership: maki evidence. Claim: unassigned.

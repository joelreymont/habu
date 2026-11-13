---
title: "Cut the warm engine build's guard, storage and path costs"
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T09:59:39.467379+02:00"
---

Problem: a warm engine build spends about a third of its 0.81 s user time outside its own work: the store guard on 54M stores (10.6%), DYNAMIC-STORAGE:ADDRESS 31M calls and RESERVE's extent division 3.2M (12.6%), realpath 5,746 calls for 390 paths with two dlsym lookups each (about 10%). Acceptance: a design Joel approves, then the same warm build measured faster by the parts it lands, with the guard's and storage checks' refusals unchanged and warm equal to cold. Depends: none. Claim: agent=astra workspace=.jj-ws.noindex/jerry-perf

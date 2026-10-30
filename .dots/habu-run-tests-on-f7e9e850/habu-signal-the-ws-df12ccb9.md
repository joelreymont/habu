---
title: Signal the ws and http test hooks in a finally
status: closed
priority: 2
issue-type: task
created-at: "2026-10-30T20:47:21.686684+02:00"
closed-at: "2026-10-30T21:41:46.277921+02:00"
close-reason: "landed master ff4b6876: a route or gated hook that throws still posts its semaphore (ws-test ROUTED/AWAIT-ROUTE, http-test GATED-START finally); reviewed, fold-ins applied; ws-test and http-test green at the tip"
---

Problem: census B section 1a: src/lib/ws-test.f ACCEPTED (6 sites) and src/lib/http-test.f:2173-2174 HOOK-READY wait on a semaphore a server worker posts; the test holds no task to watch, so a hook that throws before posting leaves the wait forever. Acceptance: the poster posts in a finally (the src/lib/pg-rows-test.f:402 shape) and the test fails on the thrown code it records. Files: src/lib/ws-test.f, src/lib/http-test.f. Verify: both files green; a planted throw in a hook ends the case at once. Depends: none. Ownership: those files. Claim: unassigned.

2026-10-30 21:30, lane: worker in .jj-ws.noindex/jerry-hook on master@origin 5cf48e78, brief /tmp/jerry-big/hook-brief.md (placeholders ROUTE-CODE, HOOK-CODE), baseline /tmp/jerry-big/hook-base. Left for the timers lane: http-test.f RELEASE-HOOKS sleeps 50 ms before reading START-DONE.

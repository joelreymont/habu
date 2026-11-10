---
title: A log-test case can fail without its label
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T11:10:14.998505+02:00"
---

Problem: in src/lib/log-test.f a failure inside OPEN-CASE or FINISH-CASE prints no case label, so the failing case cannot be told from the output; and RELEASE-CASE passes only while WRITER-A has never registered LOG's release, an order dependence on the cases before it. Found by the worker of habu-correct-log-s-6ccb5809. Source: Joel, 2026-11-10: "you must dot and launch a fix automatically for any issues you identify". Acceptance: every case sets its label before its first assertion; RELEASE-CASE uses a task of its own, as OPEN-CASE does with OPENER, so reordering the cases changes no result. Files: src/lib/log-test.f. Verify: bin/hb --load src/lib/log-test.f; the same with RELEASE-CASE moved first in LOG-RUN. Depends: the LOG fixes landed (695f3301, a696ddf9, 6ccb5809, 839239b8, 77cf71be). Ownership: LOG's tests. Claim: unassigned.

Claim: agent=bob workspace=.jj-ws.noindex/habu-a-log-test-8756554e

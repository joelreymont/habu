---
title: Drop the fake exit outcomes in two test libraries
status: closed
priority: 3
issue-type: task
created-at: "2026-10-02T05:03:44.607073+02:00"
closed-at: "2026-10-15T01:26:46.417381+03:00"
close-reason: Completed or superseded on current master. luxlmwo removes fabricated outcomes and demonstrates actual builder output/failure drive their rows. Read-only source/history evidence checked in backlog review; no new code or broader runtime claim.
---

Problem: tools/json-only-test-lib.f JOT-RUN-CORE (:144) and tools/bundle-lib-test-lib.f BLTT-RUN-BUNDLE-LIB (:142) run the tool in-process and fake `0 OUTCOME:EXITED`, so the caller's exit-0 assert can never fail (found by the r4-outcome lane, dot 4ad09793, which made tools/aot-lint-test-lib.f and tools/checked-boundary-lint-test-lib.f return a plain rc instead). Their expect helpers also serve a real child run. Acceptance: in-process runs return what they really produce (lengths, or an rc they can get wrong), real child runs keep the outcome asserts; a case shows the in-process failure is now observed. Base: after 4ad09793 lands. Verify: tools/json-only-test.f, tools/bundle-lib-test.f.

---
title: Replace model line refs with word names
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T19:29:19.793121+02:00"
---

Full context: EVERY src/core/checker.f line reference in formal/Common/Effects.v and Control.v is stale, uniformly by 55 to 100 lines — measured: CF-PUSH cited 7651 actually 7713; CF-TOK? cited 8356-8390 actually 8415-8441; MATCH-BEGIN cited 8180 actually 8279; CT-INIT cited 1121-1153 actually 1176-1208; CC-MAX cited 987 actually 1040, and a dozen more. These references are the models' only check-me-against-the-code affordance and all of them now point at the wrong place. Line numbers rot by construction, so do not merely refresh them: REPLACE the numeric references with word names and add a clause to test/compiler/checker-model-proof.f that reads each named word through the structural source reader and requires it to exist exactly once in src/core/checker.f, so the reference cannot rot again. Also fix Control.v's header, which attributes the kind-9 CF-PUSH to MATCH-BEGIN; MATCH-BEGIN only sets the match flag and the frame is opened by MATCH-FAM-TOK.

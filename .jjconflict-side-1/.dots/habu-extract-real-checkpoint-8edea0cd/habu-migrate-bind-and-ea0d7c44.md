---
title: Migrate bind and check suites to provider
status: closed
priority: 2
issue-type: task
created-at: "2026-07-26T22:41:19.100499+02:00"
closed-at: "2026-08-02T16:43:02.289301+02:00"
close-reason: authoritative ancestor 5b0ebb070a5b8ef7c04e2d28772421f796b686c6 deleted the unused GPT2LOAD/GPT2TX/WSTORE/MODELPROV host architecture and suites; retaining the task would resurrect deleted architecture.
---

Owned files: maki/infer/gpt2-bind-fixture.f (the duplicated path-and-configuration owner, line 271) and maki/infer/gpt2-check-test.f. They consume GPT2-FIXTURE for path, bytes, census, and configuration; their local copies of the pinned facts are deleted in this same commit, proven gone by boundary-aware sweep. gpt2-bind-test.f is a consumer through the fixture and is not edited except as the fixture change flows through. Acceptance: bind and check suites rc=0 through the provider; sweep table in the report; diff lints clean.

---
title: Migrate bind and check suites to provider
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T22:41:19.100499+02:00"
blocks:
  - habu-create-gpt2-fixture-63b55c1c
---

Owned files: maki/infer/gpt2-bind-fixture.f (the duplicated path-and-configuration owner, line 271) and maki/infer/gpt2-check-test.f. They consume GPT2-FIXTURE for path, bytes, census, and configuration; their local copies of the pinned facts are deleted in this same commit, proven gone by boundary-aware sweep. gpt2-bind-test.f is a consumer through the fixture and is not edited except as the fixture change flows through. Acceptance: bind and check suites rc=0 through the provider; sweep table in the report; diff lints clean.

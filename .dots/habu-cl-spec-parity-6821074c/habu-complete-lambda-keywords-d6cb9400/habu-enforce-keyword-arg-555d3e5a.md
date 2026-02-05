---
title: Enforce keyword arg validation
status: closed
priority: 2
issue-type: task
created-at: "\"2026-02-05T21:43:18.588106+01:00\""
closed-at: "2026-02-05T22:07:17.460577+01:00"
close-reason: Already implemented in prior keyword validation commit
blocks:
  - habu-add-allow-other-865a39ac
---

Context: src/interp/vm.zig:6735-6895; cause: unknown keyword args accepted; fix: reject unknown keywords unless in allowed_keywords, chunk.allow_other_keys, or :allow-other-keys t present; deps: habu-add-allow-other-865a39ac; verification: integration test errors on unknown keyword unless explicitly allowed.

---
title: Honor safety
status: closed
priority: 2
issue-type: task
created-at: "\"2026-02-05T20:11:00.481309+01:00\""
closed-at: "2026-02-05T22:17:01.996596+01:00"
close-reason: "Implemented with parse-optimize commit: safety=0 now suppresses emitted type checks"
---

Context: /Users/joel/Work/habu/src/compiler/compile.zig:3000-3115,9624; cause: assertions always emitted; fix: skip assert_* IR when safety=0, keep when safety>0; deps: habu-parse-optimize-85c7bb89; verification: test (declare (optimize (safety 0))) suppresses type error

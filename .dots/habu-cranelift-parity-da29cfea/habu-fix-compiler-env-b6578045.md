---
title: Fix compiler env restore
status: closed
priority: 1
issue-type: task
created-at: "2026-02-04T06:47:35+01:00"
closed-at: "2026-02-04T06:47:35+01:00"
close-reason: Add macro expansion global_env restore test
---

Context: src/compiler/compile.zig:2718; cause: macro expansion temporarily overrides vm.global_env; fix: restore global_env and add regression test; test: macro expansion restores VM global_env; deps: none; verification: zig build test.

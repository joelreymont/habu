---
title: Fix string seq compare
status: open
priority: 1
issue-type: task
created-at: "2026-02-01T22:29:49.670161+01:00"
---

Context: src/interp/vm.zig:6534-6624; cause: ignores cmp and invalid item silently returns nil/0; fix: use hashKeyEqualWithTest for string elements, error on invalid item; deps: none; verification: add tests in vm/primitives

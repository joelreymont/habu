---
title: Check JIT code size
status: closed
priority: 2
issue-type: task
created-at: "\"2026-02-04T07:04:10.861425+01:00\""
closed-at: "2026-02-04T07:05:22.226297+01:00"
close-reason: Add bench-check code_bytes guard
---

bench/check.zig: add max_jit_code_bytes threshold and CLI flag; use jit.code_bytes; deps: habu-add-jit-code-7ae9d76b; verification: zig build test

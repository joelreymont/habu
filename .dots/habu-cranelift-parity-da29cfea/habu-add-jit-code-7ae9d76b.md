---
title: Add JIT code size stats
status: closed
priority: 2
issue-type: task
created-at: "\"2026-02-04T07:02:34.111458+01:00\""
closed-at: "2026-02-04T07:03:40.664805+01:00"
close-reason: Expose JIT code_bytes in stats/json
---

src/interp/vm.zig: expose jit code bytes; bench/jit.zig: include code_bytes in JSON; deps: none; verification: zig build test

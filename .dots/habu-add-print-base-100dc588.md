---
title: Add *print-base*/*print-radix* support
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:35.548578+02:00"
---

src/interp/vm.zig + io.zig: Implement numeric base printing
- Add global *print-base* (2-36, default 10) and *print-radix* (boolean)
- Print integers in specified base
- Print radix prefix (#x #o #b) when *print-radix* is t
- Add tests for all bases
- Est: 20 min

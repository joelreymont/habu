---
title: Add read-from-string primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:26.453887+02:00"
---

src/runtime/primitives/io.zig: Implement read-from-string
- read-from-string: parse string as Lisp object
- Support :start/:end/:preserve-whitespace parameters
- Return (values object position)
- Use existing parser on string input
- Add tests for various Lisp forms
- Est: 25 min

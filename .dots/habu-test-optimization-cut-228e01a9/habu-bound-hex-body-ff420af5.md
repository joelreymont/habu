---
title: Bound HEX-BODY before reading a byte
status: open
priority: 3
issue-type: task
created-at: "2026-10-01T04:12:40.699209+02:00"
---

Problem: tools/code-owner-main.f:20 HEX-BODY evaluates `a c@` before its `u 1 >` bound (and does not short-circuit), so an empty argument reads a byte past the input. Acceptance: no byte is read outside the argument for any length; an empty or one-character argument is refused as code-owner refuses other bad offsets, shown by running tools/code-owner.f with those arguments. Files: tools/code-owner-main.f. Verify: the code-owner row. Depends: none. Ownership: HEX-BODY.

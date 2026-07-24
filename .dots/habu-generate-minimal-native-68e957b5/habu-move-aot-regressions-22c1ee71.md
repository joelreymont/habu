---
title: Move AOT regressions out of builds
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T20:47:07.009803+02:00"
---

Why: src/habu/aot-capture.f executes two regression fixtures during every
production stdin metabuild. They are tests, not capture prerequisites.

Outcome: this parent is an aggregate. Its children retire the redundant
protected-WID fixture, separate the stdin builder definitions from the terminal
entry without parsing source text, and move the record-WID round-trip into a
selectable proof inside the real metabuild host. The children own all code.

Acceptance: production aot-capture.f contains only capture prerequisites; the
real protected-WID warm suite and record compact/expand proof retain stronger
positive and corrupt-codec coverage; normal emitted engine bytes remain
unchanged; no copied source parser, codec, public test helper, or compatibility
entry remains.

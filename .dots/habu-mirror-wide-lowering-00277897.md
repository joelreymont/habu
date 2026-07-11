---
title: Mirror wide lowering in bootstrap
status: open
priority: 2
issue-type: task
created-at: "2026-07-11T12:43:48.970655+02:00"
---

bootstrap/cg/forth.fs lacks native wide @/! pass2 fact lookup, lowering helpers, dispatch, labels, and execution/golden coverage. Mirror src/habu/habu2.f behavior and prove a Gforth-emitted stage compiler lowers and executes W2/W4 memory bundles correctly.

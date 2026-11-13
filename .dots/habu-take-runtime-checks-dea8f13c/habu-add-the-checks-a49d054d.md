---
title: Add the --checks build option
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T10:51:44.249950+02:00"
---

Problem: AGENTS.md (Changes) allows code instrumented with bounds and other checks on demand. Joel named the option --checks and shelved it on 2026-11-13; meanwhile the lanes under habu-take-runtime-checks-dea8f13c delete those checks from default code and disable their tests with `off until dot` and this dot's id. Acceptance: tools/hb-build.f --checks builds an engine or a program whose code carries the deleted checks again (buffer index bounds, tag on fetch, MATCH fall-through, zero divisor, 0 throw, Wasm load address, library preconditions, task and AIO ownership, the linker's own state), restored from those commits; definers and library code test the choice through HB-CHECKS?, and T0 folds `HB-CHECKS? if ... then` so default code carries nothing; checked objects never share a directory with default ones, so a warm build still equals a cold one; the disabled tests run on a --checks engine. Depends: none. Claim: unassigned.

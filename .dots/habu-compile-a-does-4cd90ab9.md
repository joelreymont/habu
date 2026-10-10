---
title: Compile a does> definer with an empty head at tier 1
status: open
priority: 2
issue-type: task
created-at: "2026-10-10T07:38:37.590695+03:00"
---

Problem: a definer whose head before `does>` is empty is admitted by tier 0 and tools/check.f, but native tier 1 refuses it with an internal code and no diagnostic: ~/.cache/tmp/carl-gfrest/c/dc5.f (`: MK ( -- ) does> ( -- n ) @ ;  1 . cr`) and dc6.f (the same with `trusted:`) print `1` at tier 0 and under tools/check.f, and `ncomp: cannot compile MK`, uncaught -8550 (E-NELAB-CALL), rc 67, under the Habu loop at tier 1 (`bin/hb --load test/outer-loop-on.f test/gforth/tier-1.f <file>`). The Gforth host admits both. Found by the hand-the-rest lane's clause-record probes.
Acceptance: at tier 1, under both loops, a definer with an empty head compiles as at tier 0; dc5 and dc6 print `1`, rc 0, join a native tier-1 test and the Gforth host's test/gforth/cases/, and match there.
Files: src/compiler/native/ (the does> head's elaboration), a native tier-1 test, test/gforth/cases/.
Verify: native build per docs/gate.md; `bin/hb --load test/run.f`.
Depends: none. Worker: worker-max.
Superseded: the one-pass codegen (docs/architecture.md, "The codegen is one pass over the checked events") deletes the tier-1 code this fixes; its reproducers become that codegen's cases. Do not start.

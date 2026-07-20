---
title: Fix maki in-suite spawn env leak red-flagging data-loader
status: open
priority: 1
issue-type: task
created-at: "2026-07-20T22:51:44.352207+02:00"
---

maki/examples/nanogpt/data-loader-test.f passes standalone (6/6, quiet machine) but fails deterministically at rc 70 inside the full maki/test.f suite since the 2026-07-20 evening landings, red-flagging maki/test.f on master. The maki harness includes suite files in one process, so ~170 earlier suites' state precedes it; the test spawns child engines, and rc 70 (E-UNDEFINED class) from a child indicates the spawned engine or its load set is resolved differently in-suite - prime suspect: a PROC-ENV default (HABU_UNDER_TEST or similar) or process-argv/env table state left set by an earlier suite (candidate/replay machinery), redirecting the child to a stale engine missing newly landed words. RCA in-suite with binary member bisection (run maki/test.f with a truncated master list), identify the leaking suite, make it reset its env defaults (the structural fix is the leaking OWNER resets, not the victim tolerating), and add a harness-level invariant if cheap: assert the PROC-ENV default table is empty between suite files. Reproducers: RC_MAKI=70 in batteries bi976bl2g/bsjam1wtm and direct quiet-machine run, all on trees at/after a1392549.

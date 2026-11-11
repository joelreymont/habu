---
title: "Check a call to an FSM:DEFINE machine as the load does"
status: active
priority: 2
issue-type: task
created-at: "2026-11-11T14:18:07.229235+02:00"
---

Problem (mcall fix worker, confirmed by the lead on master 8f6ec543 with its installed engine): bin/hb --load tools/check.f -- src/lib/net/model.f is rc 67, 'hb: uncaught throw code -9660' (src/lib/fsm.f E-SYNTAX), and so is the reproducer ~/.cache/tmp/carl-hfsm/fsm-check-repro.f (an FSM:DEFINE machine M1, then ': RUN1 ( n -- ) construct sst sa M1 ;'); bin/hb --load of the reproducer is rc 0. FSM:DEFINE alone checks. So check cannot check a library the model-call clients load. Acceptance: the cause found and fixed in the layer that owns it; check of model.f and of the reproducer rc 0. Files: unknown until the hunt (src/lib/fsm.f, tools/check-core.f, src/habu/verify-source.f). Verify: the two commands; a case in src/lib/fsm.f's test. Depends: none. Ownership: check of FSM machines. Claim: agent=carl workspace=.jj-ws.noindex/carl-hfsm


Commit A landed (ptnztypx "Place a file's definitions without markers", with the batch on e22910b0). B2 and B1 (.jj-ws.noindex/carl-layout/.out/handoff-notes-42.md "hfsm commit B plan") are not started: no consumer meets this, so they wait (AGENTS.md "Work that does not move that measure waits").

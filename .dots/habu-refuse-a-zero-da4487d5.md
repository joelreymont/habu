---
title: Refuse a zero throw code
status: active
priority: 2
issue-type: task
created-at: "2026-10-28T21:14:04.201355+02:00"
---

Problem: `0 throw` unwinds but reports success: `[: 0 throw ;] catch` answers 0, and an uncaught 0 throw silently ends the program with rc 0, skipping the rest (reproducer .jj-ws.noindex/carl-throw0/.out/repro/zero-throw.f, master engine; found by tim's CMD:RUN review, probes tim-cmdrun/.out/review/probe0*.f). Ruling (Joel asked for the long-term choice; carl decided): 0 throw throws a new named nonzero engine code, not the ANS no-op, because the checker types throw as never returning and a zero code is always a caller bug. Acceptance: every engine's throw primitive replaces 0 with the new code before unwinding; catch answers it; uncaught exits nonzero naming it; nonzero throws unchanged; callers that threw 0 fixed; docs/forth.md 'Rules learned by refusal' and the card list it. Files: src/habu throw primitive bodies, the engine throw test, docs. Verify: reproducer, engine suite, full suite, x86-64 on pulsar if an x86-64 body changes. Depends: none. Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-throw0.

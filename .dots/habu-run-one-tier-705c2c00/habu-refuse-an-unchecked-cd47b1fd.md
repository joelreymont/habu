---
title: Refuse an unchecked XT @ execute at tier 1 with a diagnostic
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T19:23:10.780960+02:00"
---

Problem: under 0 set-check, tier 1 refuses `: V ( -- ) XT @ execute ;` with E-NELAB-QUOT (-8651) uncaught, rc 67; T0 and the JIT run it (T0 prints the hookless scan's reason). Repro /tmp/jerry-big/gaps/unchecked-execute-t1.f. Also `['] WIN execute` in a TRUSTED: body at tier 1, WIN a word whose effect is unrecorded: -8651 uncaught (/tmp/jerry-big/gaps/unrecorded-execute-t1.f). Acceptance: tier 1 compiles it as T0 does, or refuses with a named diagnostic, never an uncaught throw. Depends: T0 release. Claim: unassigned.

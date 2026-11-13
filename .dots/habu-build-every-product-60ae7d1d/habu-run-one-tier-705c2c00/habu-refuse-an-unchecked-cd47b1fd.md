---
title: Refuse an unchecked XT @ execute at tier 1 with a diagnostic
status: closed
priority: 2
issue-type: task
created-at: "2026-10-30T19:23:10.780960+02:00"
closed-at: "2026-11-04T11:48:49.772748+02:00"
close-reason: "Census 2026-11-04: goes with habu-delete-tier-1-f5c44fb4, T0 compiling every target, tier 1 and the JIT kept only for experiments. Tier-1 refusal defect at 1 set-tier; no Wasm/stripped path named, so the rule that tier 1 is edited only to fix a defect on a path it still owns (Joel 2026-11-03) forbids it; inferred."
---

Problem: under 0 set-check, tier 1 refuses `: V ( -- ) XT @ execute ;` with E-NELAB-QUOT (-8651) uncaught, rc 67; T0 and the JIT run it (T0 prints the hookless scan's reason). Repro /tmp/jerry-big/gaps/unchecked-execute-t1.f. Also `['] WIN execute` in a TRUSTED: body at tier 1, WIN a word whose effect is unrecorded: -8651 uncaught (/tmp/jerry-big/gaps/unrecorded-execute-t1.f). Acceptance: tier 1 compiles it as T0 does, or refuses with a named diagnostic, never an uncaught throw. Depends: T0 release. Claim: unassigned.

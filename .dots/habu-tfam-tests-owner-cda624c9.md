---
title: "tfam tests: owner-wid suites mislead when run standalone"
status: open
priority: 2
issue-type: task
created-at: "2026-07-12T22:59:23.775692+02:00"
---

Merge review 2026-07-12 MEDIUM (test-harness robustness, tfam-lane owner-seal campaign): test/owner-wid-{state,call,emitter,child}.f are build-context suites (green inside test/run.f's forge harness) but standalone 'bin/hb --load' gives confusing failures with no guard: state -> TFAIL (registry empty at cold boot), call -> E-UNDEFINED owner-wid-add, emitter -> E-UNDEFINED LIT64, (build-side assembler sealed), child -> 2-MINUTE HANG (no child-wait timebox). Violates the no-silent-misleading-failure stance. Fix: standalone-invocation guard per suite (fail-fast named message 'run via test/run.f' or auto-skip with reason) + timebox the owner-wid-child.f child wait. Owner: whoever touches the owner-seal suites next (either lane).

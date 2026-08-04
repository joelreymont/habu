---
title: Retire CHECKER-CERT-CALL trusted execute boundary
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T23:57:59.821788+02:00"
---

Split from the landed checker-exec migration. `PRODUCER-XT` and `FULL-XT` in
`src/core/lower-cert-base.f` are single-assignment producer cells whose execute
rides `TRUSTED: CHECKER-CERT-CALL`; the opaque-execute rejection therefore does
not reach them, but the boundary can now be retired.

Result: delete `CHECKER-CERT-CALL`; rework the `lower-cert-seal.f` undefine
seal and install guards that probe the raw cells; move both producers to
statically effect-known dispatch using the load-order-valid checked mechanism.
Do not weaken the boot-critical seal or fold this into another execution flip.
Any boundary that survives during the migration carries only its source-local
rationale, this retirement owner, and the focused seal test.

Acceptance: red-first fixtures prove unset, wrong-effect, replacement, and
post-seal mutation rejection through the real lower-cert path; valid producer
and full-certificate dispatch remain exact; no source definition or call to
`CHECKER-CERT-CALL` remains. Run lower-cert and seal tests, native fixpoint,
bootstrap parity, package and typed-local gates, and the full native battery.

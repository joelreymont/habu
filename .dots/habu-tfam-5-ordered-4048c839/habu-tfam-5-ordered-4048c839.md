---
title: "TFAM 5: ordered source-event replay for preverify/tools"
status: closed
priority: 2
issue-type: task
closed-at: "2026-07-08T00:00:00+02:00"
close-reason: Parent umbrella; every line item is covered by a closed sub-dot or an open follow-up. Ordered event log + loader instrumentation with kind/path/span/state-delta/multiplicity = habu-tfam-5-ordered-8d1b278e; restricted discovery pass + fresh require registry = habu-tfam-5-restricted-c8f19eac; C"/." loader rejects + dynamic-span fail-closed = habu-tfam-5-c-bfa575d2; hb-build cache keys + public-signatures consuming the event closure = habu-tfam-5-public-3a692040 with the whole-file producer redesign resolved in habu-tfam-5-event-d7618516 (source-discovery colon-body scan, EC:BUILD shared, CKT-TEST-CLOSURE-PARITY superset proof); shared checked path emitter + loader reservation = habu-tfam-5-done-027cc58d; support-form replay + preverify/--all-errors original-file redrive = habu-tfam-5-preverify-23fac8cb with constant logical-shape parity in habu-tfam-5-const-b89c90f0; Gate 17e stdin-driver-closure manifest = habu-tfam-5-stdin-manifest-9c341696 (stdin-closure-lib/lint live in the lint-tools slice). Definer-kind event remainder stays open by design in habu-definer-kind-events-64dbe6d2 (no consumer today; byte order sound). Audit gap repaired: d7618516's deferred hb-build tool key-list refinement referenced dot id habu-tfam-5-add-7730ca3e which was never minted - now tracked as habu-derive-hb-build-fa375490.
created-at: "2026-07-03T23:36:48.930009+02:00"
---

PLAN.md item 5. Generic ordered event log via runtime include/require/provided instrumentation (src/core/include.f); restricted discovery pass before static consumers; events carry kind/path/span/state-delta/multiplicity; C\"/.\" loader forms reject; hb-build cache keys consume replay closure; single checked path-string emitter shared by check-core/lib-source/materializers; support forms (deftype/VALUE-RECORD/defer/constant/create/variable/immediate/TRUSTED:/TRUST/undefine/EXPORT/package) replayed; --all-errors redrives original source-list files. First checkpoint needs no ADT grammar. Gate 17e. Depends: TFAM 2a-4.

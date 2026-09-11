---
title: Own resident CHECK gate
status: closed
priority: 1
issue-type: task
created-at: "2026-07-24T17:15:24.636628+02:00"
closed-at: "2026-07-24T18:11:55.678382+02:00"
close-reason: Reviewed implementation landed at master@origin 7170b6b6; both CHECK dispatch paths and all required gates are green.
---

Why: the resident CHECK performance correction must change GSI-CHECK-CLI, but that definition is a legacy global inside test/gate-stdlib-inline-lib.f and fails the exact package-ownership gate. Exact result: replace only the existing GSI-CHECK-CLI definition with package CHECK-CLI-GATE and public RUN ( -- ). In this ownership leaf RUN preserves the current sequential group header, tool setup, check-core setup file, tools/check-test.f include, diagnostics, timing, and failure propagation byte-for-byte. Update only its two package-owned production callers in test/gate-runner-lib.f and test/run-worker-stdlib.f to CHECK-CLI-GATE:RUN. Keep every shared GSI helper and every other resident group at its existing owner. Do not add aliases, forwarding globals, duplicate schedules, new state, CHECK shards, parity policy, selector changes, or compatibility names. Owner and files: the CHECK phase block in test/gate-stdlib-inline-lib.f and its two exact callers; FILEMAP only if its ownership description changes. Pre-change proof: a representative body change to GSI-CHECK-CLI produces E-PACKAGE-OWNERSHIP through tools/package-diff-lint.f. Acceptance: only CHECK-CLI-GATE:RUN resolves; GSI-CHECK-CLI and every qualified private package word reject; the gate-runner check-cli dispatch and resident worker identifier 3 execute the unchanged tools/check-test.f path exactly once with identical output and status; exact typed-local and package-diff gates pass. Smallest owning-path check: both real check-cli production dispatches plus direct qualified-name rejection fixtures. Dependency: none beyond the exact reviewed local claim base. This leaf blocks habu-cut-over-check-ac1b7cdf. Claim: agent=own_check_gate workspace=.jj-ws/habu-own-resident-check-022364c3.

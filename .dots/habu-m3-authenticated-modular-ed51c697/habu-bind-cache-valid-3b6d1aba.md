---
title: Bind cache validation certificates
status: open
priority: 1
issue-type: task
created-at: "2026-07-17T13:04:59.856399+02:00"
blocks:
  - habu-build-exact-modular-44f4c2dc
---

Review blocker: hb-build can restore an artifact before lints while its keys omit aot-lint/signature-lint implementation identities, so strengthened validation can be bypassed by a hit. Bind artifact reuse to a checked validation certificate keyed by every lint/checker/hook/source/provider identity and exact executable digest, or run validation before restore. Add mutation fixtures for each validation input and truthful hit/build reports. Files: tools/hb-build-lib.f/report/tests and canonical cache schema only; no scanner/frame edits. Acceptance: stale validation evidence always misses or revalidates; focused hb-build/cache and full gates green.

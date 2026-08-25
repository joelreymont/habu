---
title: Scale tail ratchets with gate calibration
status: closed
priority: 1
issue-type: task
created-at: "2026-07-17T20:04:25.071805+02:00"
closed-at: "2026-07-17T20:44:21.596278+02:00"
close-reason: "Landed aabd42aa: measured performance ratchets are separate from structural child timeouts; nominal 8/10s, measured 16/20s, and 30s nested timeout regressions are green; full cold gate was 22.99s; two independent reviews clean."
---

Full context: final destruction review found test/gate-tail-process.f raw 10000ms and test/tail-ratchet.f raw 8000ms remain fixed while test/run-lib.f host-load calibration scales the owning suite budget up to 3x. Cause: nested tail deadlines bypass the selected host/profile calibration, so a healthy loaded run can fail inside tail-process while the overall calibrated 30/35s budget remains valid. Fix: expose one typed calibrated deadline policy from run-lib/profile state, apply it to the tail worker and ratchet callers without weakening nominal 8/10s limits, and add a >100% HB_LOAD_PCT regression. Acceptance: focused tail/runner calibration tests, lint-tools, typed-local/host/filemap/dot lints, maki, ptx-stdlib, and exact full native cold gate green within calibrated 30s policy.

---
title: "Restore a WITH-IO caller only to its own device"
status: active
priority: 2
issue-type: task
created-at: "2026-11-18T20:19:25.237783+01:00"
---

Problem: src/lib/genio-core.f WITH-IO restores the caller's devices through RESTORE-IN and RESTORE-OUT (:309-315), which check only that the saved device's row is LIVE. When the body closes the caller's device and opens another, CLAIM-ROW gives the new device that row, so the restore reaches ROW's generation check and throws E-STATE over the body's result, where the comment above them promises the caller goes back to the terminal. Acceptance: a genio test in which the body closes the caller's output device and opens another in its row returns from WITH-IO, leaves the caller on the terminal, and fails with the engine before the fix. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-gio.

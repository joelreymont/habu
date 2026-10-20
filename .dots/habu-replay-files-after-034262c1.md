---
title: Replay FILEs after native build prefix
status: active
priority: 0
issue-type: task
created-at: "2026-10-15T08:06:11.478745+03:00"
---

Observed matching native FILE builds use identical keys for all116 units; first enums HFO opens successfully and target/session serials both64, but SELECT-REGISTRY-READY? refuses the root FILE solely because a compiler session already exists from the native build prefix. The unchanged warm build therefore compiles all116 units. Identify and preserve the actual incoming session/registry identity boundary; permit safe existing-session replay without accepting unrelated live compiler state. Own tools/native-file-cache.f and necessary prefix/registry interface only. Acceptance: existing native-build-edit E2E gets real warm hits and correct runnable products, root/leaf edits invalidate required dependents, unrelated session state stays declined, Astra review and required matching native/convergence gates. Do not weaken registry checks merely on serial equality.

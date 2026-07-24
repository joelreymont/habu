---
title: Reset declaration events after identity restore
status: active
priority: 1
issue-type: task
created-at: "2026-07-24T21:26:49.995411+02:00"
---

Why: test/enum-decl-suite.f restores family, variant, schema, and field registry counters after the final named-payload identity comparison but leaves the corresponding published declaration-event rows live. The next declaration reuses the retired numeric family identifier and the ownership guard correctly rejects it as a duplicate. Owner: test/enum-decl-suite.f only. Exact result: after the final REG-RESTORE in the named-payload identity block and before the reserved-name package tests, call the existing public DECL-EVENT:RESET exactly as the preceding identity repetitions already do. Acceptance: the final identity comparison remains executed before reset; the following foreign-variant declaration uses a fresh event log; removing only this reset reproduces E-DEV-FAMILY-SCOPE 7173 under the ownership guard; every registry snapshot, identity assertion, and reserved-name assertion remains unchanged. Forbidden: production edits, REG-RESTORE expansion, private event access, new reset seam, changed identity inputs, reduced assertions, or legacy declaration changes. Smallest check: bin/hb --load test/enum-decl-suite.f. Depends: none. Claim: agent=enum_identity_reset workspace=.jj-ws/habu-reset-declaration-events-71ec9f20.

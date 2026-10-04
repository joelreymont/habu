---
title: "Bring master's new records into the diagnostic contract"
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T23:12:52.748391+02:00"
---

Problem (merge 53e71537): master's E-TRUST-UNRESOLVED and E-PKG-CONTEXT records are not covered by tools/gate-json-assert-core.f GJA-DIAG-SHAPE or the repair-packet shapes (tools/repair-packet-core.f), so their JSON shape is unchecked. Acceptance: both are in the contract (docs/repair-diagnostics.md, GJA-DIAG-SHAPE, repair class), a gate-diagnostics case emits each through tools/check.f --json-errors and passes DIAG-CONTRACT, seen failing first. Files: tools/gate-json-assert-core.f, tools/repair-packet-core.f, docs/repair-diagnostics.md, test/gate-diagnostics-lib.f.

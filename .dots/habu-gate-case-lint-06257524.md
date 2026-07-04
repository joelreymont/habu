---
title: Gate case lint-tools/dot-maki hides which sub-suite failed
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T22:59:26.328758+02:00"
---

Diagnosability gap found 2026-07-04: the gate case 'lint-tools/dot-maki' (test/gate-stdlib-lint-tools.f GSI-LINT-TOOLS-DOT-MAKI) bundles dot-dep-lint + maki-dep-lint + host-lint-test + trusted-inventory-test; a failure reports only 'FAIL: lint-tools/dot-maki' + a bare 'TFAIL assert N'. A trusted-inventory ratchet drift (habu2.f 102->103) was mis-attributed to dot-graph churn THREE times (two worker reports + one orchestrator pass) because the case name says dot-maki and the log's last informational lines were the dot lint's counts. Fix: split the case into one fork per sub-suite (lint-tools/dot, lint-tools/maki, lint-tools/host, lint-tools/trusted-inventory) OR make GSI-INCLUDE label assertion failures with the including suite path. Files: test/gate-stdlib-lint-tools.f (+ label allowlist/TEST:SUITE rows per the dual-path lesson in LESSONS.md).

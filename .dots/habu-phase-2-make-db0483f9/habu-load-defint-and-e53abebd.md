---
title: Load defint and residu on clean path
status: closed
priority: 1
issue-type: task
created-at: "\"2026-04-01T22:06:02.254396+02:00\""
closed-at: "2026-04-04T17:10:45.973458+02:00"
close-reason: "done earlier: src/tests/integration.zig has maxima loader binds residu and defint after core bootstrap; both modules load on the clean path and expose maxima::$residue / maxima::$defint"
blocks:
  - habu-propagate-load-ctx-181876c0
---

Problem: defint.lisp and residu.lisp remain excluded by unresolved Habu gaps. Acceptance: both modules load on the clean path with focused regressions. Files: current failing upstream modules and proven Habu subsystems. Verify: focused module loads plus regressions for the fixed root cause. Blockers: habu-propagate-load-ctx-181876c0; also depends on habu-make-break-warn-41be2573.

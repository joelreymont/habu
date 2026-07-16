---
title: Close PTY test trust scan gap
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-16T13:16:13.409376+02:00\""
---

Full context: tools/refine-lint-core.f:506-514 excludes test/, while TRUSTED.md claims private PROCESS-PTY conversion confinement and test packages can reopen PROCESS-PTY. Fix: scan test/ in refine-lint; allow only exact TRUSTED.md-cited PTY tests; add tools/refine-lint-test.f negative fixture proving an uncited test path is rejected. Dependencies: PTY private conversion inventory from habu-fix-candidate-pty-9a3bf504. Acceptance: focused refine lint/test, trust inventory, typed diff, host/filemap gates green.

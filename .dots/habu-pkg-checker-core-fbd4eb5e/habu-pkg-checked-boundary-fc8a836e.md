---
title: Package checked-boundary lint core
status: open
priority: 1
issue-type: task
created-at: "2026-07-23T02:10:09.049031+02:00"
blocks:
  - habu-own-checked-hook-d1588988
  - habu-pkg-checked-boundary-eb121cc5
---

Why: tools/checked-boundary-lint-core.f is a package-less UB vocabulary, and its name-only hook allowlists accept wrong-file installs. Dependencies: the HOOK-SITES registry and packaged checked-boundary test library. Owner: package CHECKED-BOUNDARY-LINT. Files: tools/checked-boundary-lint-core.f, tools/checked-boundary-lint.f, tools/checked-boundary-lint-test-lib.f. Publish exactly RESET, JSON!, STRICT!, OUT-FD!, FILE, and FINISH; keep the command MAIN private. Remove all UB-* globals, UB-HOOK-ALLOWED?, and UB-TOP-HOOK-ALLOWED?. Consume HOOK-SITES directly; do not copy registry data or parse TRUSTED.md independently. Acceptance: real CLI and suite reject wrong file/name/kind, comments, strings, duplicate/reordered tick forms, qualified spoofing, path aliases, stale registry rows, and count drift; every audited current hook passes; no alias, tail-only comparison, exported state, or compatibility global. Pre-change proof: changing the global core fails E-PACKAGE-OWNERSHIP and a wrong-file named hook passes. Verify through the production CLI, packaged test library, trusted inventory, exact diff ownership/type, host, and file-map gates.

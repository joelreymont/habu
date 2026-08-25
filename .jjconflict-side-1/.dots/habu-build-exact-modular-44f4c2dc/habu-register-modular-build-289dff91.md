---
title: Register modular build gates
status: open
priority: 1
issue-type: task
created-at: "2026-07-15T23:51:23.333080+02:00"
blocks:
  - habu-cut-hb-build-6e53c639
---

Full context: source-map and diagnostic-remap tests need production execution after every modular source, map, diagnostic, and HB-BUILD package leaf lands. Register each focused test in its real owning gate, keep maker/source closure inputs and cache keys exact, and prove each dedicated test is observed once through that production path. Acceptance: every dedicated test runs once in its owning suite; typed-local, reserved-name, host, dot, Maki, PTX standard-library, fixpoint, and full cold/hot native gates pass on the exact rebased tree. Full fan-in prerequisites: habu-remove-synthetic-compose-373b117a, habu-validate-canonical-src-3fbbcf67, habu-reuse-src-composition-33a16ba8, habu-cross-check-remapped-12125855, habu-quote-diagnostic-paths-5e982e5e, habu-idx-src-map-dfad08a6, habu-grow-diagnostic-remap-79bc5391, habu-reuse-src-map-0a657e25 and habu-cut-hb-build-6e53c639.

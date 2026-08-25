---
title: Per-process cad-store isolation in tests
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T17:44:18.066427+02:00"
---

Review finding 4 (pin 8195257e): maki/store.f:151 resolves tmp/cad-store repository-relative and :295 recursively deletes the whole root; the gate sets HB_TMP but not HABU_CAD_STORE (test/run-lib.f:1839). Two concurrent cad-test.f runs in one tree: one pass, one nine-failure run. Fix: an owned per-process test-store capability - each test run gets its own store root (derive from HB_TMP + pid or a passed capability), deletion bounded to the owned root, gate exports it. Concurrent gates in one tree are otherwise a standing flake source.

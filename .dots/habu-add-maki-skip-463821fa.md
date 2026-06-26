---
title: Add maki skip to host-lint .py reject
status: open
priority: 2
issue-type: task
created-at: "2026-06-26T23:57:38.027321+02:00"
---

tools/host-lint.f:132 walks the whole repo (s" ." WALK-FILES) and :88 HOST-PATH-BAD? rejects any .py path; it runs in the gate (test/gate-stdlib.f host-lint). The moment maki/ ships any .py (ONNX/eval tooling), the native gate fails. Preferred fix per AGENTS.md Habu-Only: maki ships NO .py at all (document it). If a tracked host-glue .py boundary is ever unavoidable, add a maki/-skip in HOST-SCAN-FILE as a deliberate, audited exception WITH a test - note that editing host-lint to special-case maki itself pierces the one-way fence.
- Files: tools/host-lint.f:88,122,132.
- Verify: a maki/ tree with no .py passes; if a skip is added, a fixture .py under maki/ is skipped while a .py under src/ still throws.
- Dep: relevant once maki/ scaffold exists.

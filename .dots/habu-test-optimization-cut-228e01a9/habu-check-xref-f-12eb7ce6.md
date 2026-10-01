---
title: Check xref.f on its own
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T11:34:48.897010+02:00"
---

Problem: bin/hb --load tools/check.f -- src/habu/xref.f exits 1 with E-RESERVED-DEFINITION at xref.f:618 on 'undefine' (same at the round-4 base); the file loads and works inside the engine build. A checked library file must pass check.f alone or say in its header which entry file checks it. Acceptance: check.f on xref.f passes, or the refusal is shown correct and the file's header names its check entry the way aot-capture.f's does; tools/check-test case if the checker was wrong. Files: src/habu/xref.f, tools/check-core.f.

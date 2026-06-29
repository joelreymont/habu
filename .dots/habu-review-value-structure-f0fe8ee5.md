---
title: Review value-structure use
status: open
priority: 2
issue-type: task
created-at: "2026-06-29T17:53:38.678258+02:00"
---

Problem: recent bootstrap-safe rewrites introduced/kept parallel scratch variables and ad hoc field helpers in stage and tool code; places such as src/habu/habu1.f LNX-* spawn state, src/os/image-bytes.f M-* byte cursor scratch, src/habu/treeshake.f SHK*/KEEP*, src/habu/crash.f CRH*, src/habu/hide.f BFR*, and src/habu/xref.f XREF* should be audited for Habu value structures from src/core/structures.f where a typed record reduces trust and makes ownership/layout explicit. Fix: review current scratch-variable groups and table/record helpers, replace applicable parallel variables with structure/value records and checked accessors, leave only measured hot-path or true boundary cases as variables with rationale. Acceptance: audit note in dot with keep/convert list; converted code has typed stack effects, no new unmanifested TRUST rows, focused fixtures plus build-fixpoint-test pass; if a structure capability is missing, add a child dot with exact checker/tooling gap instead of using a workaround. Files: src/core/structures.f, src/habu/habu1.f, src/os/image-bytes.f, src/habu/treeshake.f, src/habu/crash.f, src/habu/hide.f, src/habu/xref.f, TRUSTED.md, docs/forth.md.

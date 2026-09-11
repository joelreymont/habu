---
title: Remove native source recompilation
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:59:39.067773+02:00"
blocks:
  - habu-lower-wide-mem-ed25e3e7
---

Full context: complete Wave 6 by deleting pass-2 source recompilation and routing every wide layout from the one source tape/HIR through representation lowering. Acceptance: a gate detects any second parse/compile path; all existing wide-layout tests pass; one HIR/input digest owns every output and witness.

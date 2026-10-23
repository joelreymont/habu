---
title: Align test recipes with impact rules
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:42.001984+03:00"
closed-at: "2026-10-16T11:22:56.982162+03:00"
close-reason: Aligned the gate recipe with authoritative impact-based testing, retained source-matched builds for baked behavior and required full/convergence/recovery checks, and removed superseded MISSING policy. Reviewed against AGENTS and docs/gate; all relative links resolve. Documentation-only change.
---

skills/habu-gate/SKILL.md unconditionally builds/gates even focused tasks, conflicting with AGENTS testing impact policy;  Make recipe conditional and remove duplicate policy prose. Preserve mandatory checks for shared compiler/checker/runtime/capture changes. Documentation-only crosscheck against AGENTS; do not add governance.

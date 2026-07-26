---
title: Intern compiler attributes
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:54:58.846831+02:00"
blocks:
  - habu-intern-compiler-types-bf952f0f
---

Full context: design section 6.3 requires typed canonical attributes rather than byte/text conventions. Add closed attribute schemas, structural interning, target/numeric ownership, and deterministic reference identity. Acceptance: identical attributes intern; unknown kind, bad payload/type, illegal target, and cross-owner references reject; every attribute field participates in canonical identity. Dependency: compiler types.

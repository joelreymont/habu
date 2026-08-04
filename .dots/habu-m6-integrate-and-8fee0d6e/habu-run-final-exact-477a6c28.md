---
title: Run final exact gates
status: open
priority: 1
issue-type: task
created-at: "2026-07-17T13:05:00.726480+02:00"
blocks:
  - habu-freeze-final-change-6dc79942
---

Rebase the reviewed candidate onto current master without dropping milestone content; rebuild native bin/hb; run every leaf-focused production test plus full native test/run.f budget, Maki, PTX-stdlib and touched native slices, bootstrap recovery, byte-identical fixpoint, typed-local and package diff lints, and dot-dep-lint on that exact revision. Any failure returns to review/fix; skipped evidence is failure.

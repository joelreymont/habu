---
title: Comments say a process starts on the JIT
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T10:17:29.201271+02:00"
---

Problem: src/habu/layout.f:652-654 ("On an engine whose tier 0 is the JIT ...") and :659 ("0 JIT or no compiler") still describe tier 0 as the JIT; the JIT is deleted and tier 0 is the compiler in COMP-CELL (T0). Acceptance: both comments say what runs, or go with the code the JIT deletion removes. Depends: goes with habu-delete-the-jit-d9fdaf67's lanes. Claim: unassigned.

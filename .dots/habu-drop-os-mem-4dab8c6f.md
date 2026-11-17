---
title: "Drop OS-MEMORY's page-size check"
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T10:58:12.764087+01:00"
---

Problem: src/lib/os-memory.f PAGE-SIZE-CHECK throws E-MEM-SIZE when getpagesize answers 0 or less, which neither Linux nor macOS can do; it is a runtime check (AGENTS.md: none) and gives E-MEM-SIZE, a code for a caller's bad size, a cause it does not describe. Acceptance: PAGE-SIZE is PAGE-SIZE-CALL with no check, PAGE-SIZE-CHECK is deleted, and os-memory.f requires error.f only if something else in it uses it. Depends: none. Claim: unassigned.

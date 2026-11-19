---
title: Mark a part reading the clock or pid
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T13:53:39.747263+01:00"
---

Problem: `epoch-seconds`, `mono-ns` and `getpid` at a part's top level read the building process, so a warm build links the old value where a cold build reads a new one. Acceptance: a part whose compile runs one never links (COMPILE-READS:MISS), as `ndict@` will. Depends: habu-bake-the-pos-ef009318's first lane (the typed global wrapper in src/core/compile-reads.f). Claim: unassigned.

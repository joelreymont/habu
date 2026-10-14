---
title: Attribute baked library code sizes
status: open
priority: 2
issue-type: task
created-at: "2026-10-14T17:03:00.515478+03:00"
---

Otto’s /tmp/otto-bb-zzu.md reports tier-census only sizes newly loaded definitions and cannot attribute about15KB of reached engine-baked library code in zzusage; stripped hb-build emits no word-address map. Provide attribution from authoritative captured word ranges/names, including baked string/memory/errors/ieee754/image-lifecycle/adt words, rather than nearest-address guesses. Preserve stripped executable behavior and keep optional analysis outside product payload. Acceptance: exact per-word totals reconcile final code range and known baked/new words on a real stripped artifact. Distinct from debugger DWARF task1b095628.

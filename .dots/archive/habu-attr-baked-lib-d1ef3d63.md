---
title: Attribute baked library code sizes
status: closed
priority: 2
issue-type: task
created-at: "2026-10-14T17:03:00.515478+03:00"
closed-at: "2026-10-16T23:46:08.844436+03:00"
close-reason: "Existing public engine-size and application maps now attribute the retained stripped code. Re-ran tools/engine-size.f on the real Dots artifact with the matching c739 donor: exit 0; 161700 code bytes reconcile exactly as 16528 baked, 136268 loaded, 2828 generated and 6076 named seed/helper bytes. Baked STRING, FFI, MEM, lifecycle and numeric words are named from their captured ranges; no nearest-address guess or new product payload was added."
---

Otto’s /tmp/otto-bb-zzu.md reports tier-census only sizes newly loaded definitions and cannot attribute about15KB of reached engine-baked library code in zzusage; stripped hb-build emits no word-address map. Provide attribution from authoritative captured word ranges/names, including baked string/memory/errors/ieee754/image-lifecycle/adt words, rather than nearest-address guesses. Preserve stripped executable behavior and keep optional analysis outside product payload. Acceptance: exact per-word totals reconcile final code range and known baked/new words on a real stripped artifact. Distinct from debugger DWARF task1b095628.

---
title: Store into a created table at interpret level on x86
status: open
priority: 2
issue-type: task
created-at: "2026-11-01T20:15:09.355646+02:00"
---

Problem: on older x86-64 engines, `0 STR-MAX-I64$ !` at interpret level after `require src/lib/string.f` failed with 'hb: interpret stack underdepth: STR-MAX-I64$' (STR-MAX-I64$ is the table `create`d at src/lib/string.f:72). On arm64 `require src/lib/string.f 0 STR-MAX-I64$ ! STR-MAX-I64$ @ . depth .` prints 0 and 0, rc 0, so the shared interpret loop handles it and an x86 failure would lie in x86-only code. Whether x86 still fails is unknown; the cause is unknown (the x86 loader, the created word's effect, or the interpret depth check). The x86-64 tests are disabled until habu-re-enable-the-959502b5. This is an acceptance case of the parent (habu-move-the-interpreter-d35780fa): fix it in the code that stays, never in the machinery the parent replaces.
Acceptance: the case above, under bin/hb --load, is run on an x86-64 Linux host; if it fails, the cause is reported at file:line and fixed there, and the case prints 0 0 with rc 0 on x86-64 and arm64; if it passes, the dot closes with that run as evidence.
Depends: none.
Claim: unassigned.

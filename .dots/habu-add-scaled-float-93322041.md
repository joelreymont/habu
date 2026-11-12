---
title: Add FMATH:HYPOT3, a scaled three-component norm
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T14:59:16.926222+03:00"
---

Problem: Maki's src/trim.f trim-stretch carries its own NORM3 ( r r r -- r ), and src/lib/fmath.f has no generic equivalent. Source: "Jorge Maki reports concrete generic float copies in src/trim.f trim-stretch: BIGGER(r r--r) chooses larger finite value, NORM3(r r r--r) scales by largest absolute component before squares to avoid avoidable overflow/underflow."

Acceptance: public FMATH:HYPOT3 ( r r r -- r ) in src/lib/fmath.f that scales by the largest absolute component, so norms of tiny and huge representable components come out finite and correct whenever the true result is representable; its zero, finite, infinity and NaN behaviour is documented beside the word and consistent with the library (C's hypot returns +infinity when any input is infinite, even beside a NaN). The qualified source is handed back for Maki to migrate; no Maki edits here.

Depends: none.

Claim: unassigned.

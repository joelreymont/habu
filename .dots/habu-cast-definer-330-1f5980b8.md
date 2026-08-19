---
title: "Cast definer: 330 nominal casts want one declaration form"
status: open
priority: 2
issue-type: task
created-at: "2026-08-19T10:05:19.378520+02:00"
---

Phase 2 of 4fd12d60: 330 TRUSTED: sites are nominal identity casts - 232 with literally empty bodies, 98 pure stack shuffles (roles.f 34 - which GENERATES the TRUSTED: text at roles.f:40 - process-pty-handle.f 18, cad-num-types.f 12, maki/ 111, tail 96). Build a checked cast-declaration form: a definer that states from-type/to-type and mints the identity with the checker enforcing representation-compatibility (same cell count/roles), replacing trust with a structural check. The roles.f generator then emits the new form. Blocks the final deletion.

Probe lead (2026-08-19): a CAST: definer already exists - tools/judge/cost.f:155
uses `CAST: REAL-BITS ( r -- n ) ;`. Find its definition and semantics FIRST;
the capability may be extending CAST: with representation-compatibility
enforcement rather than minting a new form.

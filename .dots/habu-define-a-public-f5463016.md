---
title: Define a public structure in a package with a part
status: active
priority: 2
issue-type: task
created-at: "2026-11-03T13:32:02.073077+02:00"
---

Problem: a public family's generated words are recorded and defined by their source spelling, so source rules place them: in a part (/tmp/jerry-w188/v/v3.f 'package AA package BB public STRUCTURE inner FIELD x n ;STRUCTURE private ;package ;package') the name AA:BB-INNER:MAKE has two colons, CHECKER-RECORD-SYM (src/core/sumtype.f TDPLAN-NAME+) answers 0 for it, and TDPLAN-SYM+ takes 0 as a symbol already planned: 'generated declaration name already defined' at `inner`. In a package that has a part named like the constructor package (/tmp/jerry-w188/v/e1.f, QZ with part QZ-PT) the part-first qualified lookup puts QZ-PT:MAKE in the part, so the top-level call is E-UNDEFINED. Acceptance: generated public words are recorded and defined exactly in their constructor package, as TDGEN-DRV-CALL already calls them; v3.f loads and BB-INNER:MAKE runs inside AA (a part's are the part B-FAMILY of A, reached inside A only); e1.f's top-level QZ-PT:MAKE runs; docs/forth.md states the part case. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w189.

Consumers: no Maki package that has a part declares a public structure.

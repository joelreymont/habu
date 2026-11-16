---
title: Place each use in a kinds rejection
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T04:31:11.920359+01:00"
---

Problem: a type parameter used in two kinds is rejected at the family name only, with 'type parameter has incompatible kinds' and the help 'use each type parameter in one kind of place: a value type, a scope or a region' (src/core/decl-event.f REQUIRE-KINDS, called by structure-decl.f SD-SEAL and enum-decl.f ED-CLOSE). The record names neither the parameter nor the two uses, and for 'STRUCTURE sty<a,q> FIELD x a FIELD w read-view<q,q,a> ;STRUCTURE' (and the same fields in an ENUM, /tmp/jerry-kinds/ety.f, sty.f) both uses of a read as value types, so the help names no change. Acceptance: the record is placed at the later conflicting use; its label names the parameter, the kind that use demands and the field whose earlier use fixed the other kind; its help names a change that loads; a declaration-suite case per definer pins it. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-w179.

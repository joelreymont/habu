---
title: An empty type application is handled three ways
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T19:00:46.880900+02:00"
---

Problem: 'X<>' is accepted in an effect for a zero-arity family (h1.f: 'NEWTYPE zt' / ': E1 ( zt<> -- zt<> ) ;' loads), refused in a field type by SD-APP-ARGS ('too many arguments' at arity 0, 'malformed' at arity 1 or more: h3.f 'NEWTYPE zt' / 'STRUCTURE hb3 DERIVE init FIELD f zt<> ;STRUCTURE' is refused 'field type has too many arguments at 'zt<>'', m5.f 'NEWTYPE ph<a>' / 'STRUCTURE hb5 DERIVE init FIELD f ph<> ;STRUCTURE' 'malformed field type application at 'ph<>'', both rc 70 under bin/hb --load), and accepted for any arity by the loose field grammar for a family that may still arrive (src/core/structure-decl.f SD-LOOSE-APP :173-177), which defers it instead of refusing. Acceptance: one rule for an empty application in effects, field types and the loose grammar, with a refused program per place it is refused; the comment above SD-LOOSE-NODE-IMPL (:206-207) says 'the same text' again; h1.f, h3.f and m5.f agree. Depends: none. Claim: unassigned.

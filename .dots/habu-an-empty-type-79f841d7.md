---
title: An empty type application is handled three ways
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T19:00:46.880900+02:00"
---

Problem: 'X<>' is accepted in an effect for a zero-arity family (probe h1.f), refused in a field type by SD-APP-ARGS ('too many arguments' at arity 0, 'malformed' at arity 1 or more: h3.f, m5.f), and accepted for any arity by the loose field grammar for a family that may still arrive (src/core/structure-decl.f SD-LOOSE-APP :173), which defers it instead of refusing. Probes: ~/.cache/tmp/carl-fptr-review2 (re-review of the field-pointer fold-ins, 2026-11-11). Acceptance: one rule for an empty application in effects, field types and the loose grammar, with a refused program per place it is refused; the comment above SD-LOOSE-NODE-IMPL says 'the same text' again. Files: src/core/structure-decl.f, the effect parser, test/structure-decl-suite.f. Verify: the suite rc 0; h1.f, h3.f, m5.f agree. Depends: none. Ownership: unassigned. Claim: unassigned.

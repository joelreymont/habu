---
title: No definer takes a keyword or a number as its name
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T15:17:16.574410+01:00"
---

Problem: docs/forth-card.md section 1 says every definer refuses a keyword or number-shaped name; ARENA (src/core/arena.f RUN), HEAP (src/core/dynamic-storage.f DECLARE-HEAP), DEFLINEAR and VALUE-RECORD (src/core/roles.f) take 'if' and '42' with rc 0 under all three readers, because DEF-WALL judges only the words a definer makes; DEFTYPE, NEWTYPE, SUMTYPE, ENUM and STRUCTURE reject them as E-BAD-DECLARATION 'reserved name', not the card's E-RESERVED-DEFINITION. A variant name (src/core/type-family.f VARIANT-REQUIRE) refuses a keyword with E-TDECL-NAME and takes a number-shaped one; a field name (PF-NAME-REQUIRE) refuses a control word with E-PF-NAME. A plain field defines no word, while a public family's DERIVE addr accessor defines one named by the field. Acceptance: every definer, every variant name and every DERIVE addr accessor a declaration makes rejects a keyword name E-RESERVED-DEFINITION and a number-shaped name E-NUMERIC-DEFINITION at the name, the same under --load, check.f and --verify-only; a field that defines no word takes any name. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-kd.

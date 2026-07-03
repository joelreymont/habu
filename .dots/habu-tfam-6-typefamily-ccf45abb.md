---
title: "TFAM 6: TYPEFAMILY/SUMTYPE declaration grammar"
status: open
priority: 2
issue-type: task
created-at: "2026-07-03T23:36:48.933257+02:00"
---

PLAN.md item 6. Package-aware definers TYPEFAMILY, SUMTYPE, VARIANT ... END-VARIANT, END-SUMTYPE (reserved tokens; src/core/sumtype.f new file). Token grammar rejects delimiters/control words/empty sums/uppercase-mixed tails/reserved type tokens/unknown payloads/injection shapes; PKG:result<n> accepts, PKG:Result<n> rejects; multi-error mode reports+rolls back bad declarations without fake signatures. Gate 17f. Depends: TFAM 2a-5.

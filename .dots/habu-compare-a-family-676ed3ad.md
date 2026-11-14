---
title: "Compare a family-typed field through its family's derived words"
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T06:05:50.130312+02:00"
---

Problem: src/core/sumtype.f TDECL-DERIVE-NODE-OK (:814-818) admits a field or payload whose family derives anything, and the comment at :759-760 says the family's words are the field comparator, but the generators compare such a field only by its TAG (TDGEN-UNBIND1) or bind it as n (TDGEN-BINDS). Wrong answer: SUMTYPE rvsum DERIVE eq VARIANT rnil ;VARIANT VARIANT rpair n n ;VARIANT ;SUMTYPE, then PRODUCT rvpw DERIVE eq FIELD s rvsum ;PRODUCT: products holding 1 2 RPAIR and 1 3 RPAIR compare equal while RVSUM:EQ answers false. Admitted then refused: ENUM rve DERIVE eq red green ;ENUM with a SUMTYPE DERIVE eq VARIANT hold rve (at ':}' expected: n actual: rve); PRODUCT rvin DERIVE eq FIELD u n ;PRODUCT with PRODUCT rvout DERIVE eq FIELD w rvin ;PRODUCT (undefined RVIN:TAG). Acceptance: a family-typed field or payload compares and hashes through its family's derived EQ and HASH (a payload-free family may keep its TAG), and a declaration whose field family lacks a derivation the outer asks for is refused at the declaration, naming the field; each reproducer above, as a test, answers right or is refused at the declaration. Depends: none. Claim: unassigned.

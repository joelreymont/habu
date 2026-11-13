---
title: "Say DEFTYPE, not newtype, in a refused DEFTYPE's packet"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T13:29:30.039870+02:00"
---

Problem: TDECL-DEFFAMILY (src/core/sumtype.f) hard-codes decl "newtype", so a refused DEFTYPE name reports decl newtype (lane carl-pp2, change zqyowykn); on the installed hb (2026-11-13) `DEFTYPE if` under tools/check.f --verify-only --json-errors gives E-BAD-DECLARATION with "decl":"newtype", as `NEWTYPE if` does. Acceptance: the packet's decl names the definer the source used (deftype for DEFTYPE, newtype for NEWTYPE). Files: src/core/sumtype.f TDECL-DEFFAMILY and its callers. Verify: `DEFTYPE if` and `NEWTYPE if` under tools/check.f --verify-only --json-errors show decl deftype and newtype. Depends: carl-pp2 landed. Ownership: sumtype.f. Claim: unassigned.

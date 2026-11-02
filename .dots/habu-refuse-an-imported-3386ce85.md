---
title: Refuse an imported family a qualified linear names
status: open
priority: 3
issue-type: task
created-at: "2026-11-02T15:44:23.102153+02:00"
blocks:
  - habu-resolve-a-qualified-30078ff4
---

Problem: a family declared from source is refused while a DEFLINEAR or VALUE-RECORD spelled <last segment of its package>:<tail> exists (src/core/type-family.f FAMILY-RESERVED?, QUAL-NOMINAL?; lane habu-resolve-a-qualified-30078ff4), but a family arriving by unit import (checker.f UNIT-FAM-READ-KEYS) checks only for duplicate keys, so an imported unit family can land beside an existing B:X linear and shadow it inside B's parent package, letting a linear value be dropped (source form: /tmp/jerry-ptrev2-tmp/r6.f, r7.f). Acceptance: unit import refuses such a family with the same rule and error as the source path, one copy of the rule. Files: src/core/checker.f (UNIT-FAM-READ-KEYS), src/core/type-family.f. Verify: a rejected program that imports a unit declaring PKG family X after DEFLINEAR B:X, through the loader; check-cli-boundary; type-decl. Depends: habu-resolve-a-qualified-30078ff4. Ownership: unit family import. Claim: unassigned.

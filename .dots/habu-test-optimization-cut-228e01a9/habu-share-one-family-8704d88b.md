---
title: Share one family-taken check in TYPE-NAME
status: closed
priority: 4
issue-type: task
created-at: "2026-10-02T05:08:51.592974+02:00"
closed-at: "2026-10-14T19:30:56.936520+03:00"
close-reason: "Implemented in c3ff8f38: TYPE-NAME exports one FAMILY-TAKEN? scope predicate and the SUMTYPE variant gate uses it; focused suites and before/after refusals match, with rebuild convergence."
---

Problem: src/core/sumtype.f:219 TDECL-FAM-TAKEN? is a copy of src/core/type-family.f:2507 FAMILY-TAKEN? (private to TYPE-NAME): same body, global TFAM-FIND-IN then the active package's rows. Two copies of one scope rule drift, which is how ENUM and STRUCTURE lost the value-record row (dot 6d9d5656). Found by the r4-vrecname lane (49831789), which made the reserved-name list one public TYPE-NAME:FAMILY-RESERVED? and left this copy. Acceptance: one public predicate in TYPE-NAME; SUMTYPE/PRODUCT/NEWTYPE call it; TDECL-FAM-TAKEN? is gone; existing duplicate-family cases in test/type-decl-suite.f and test/type-family-suite.f stay green (no behaviour change, so no new failing case). Base: after 49831789 lands. Baked: rebuild, g1 == g2 with .names, two-generation build.

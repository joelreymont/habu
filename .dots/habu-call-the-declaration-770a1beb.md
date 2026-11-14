---
title: Call the declaration field cleanup and release plainly
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T10:47:36.051220+01:00"
---

Problem: src/core/type-family.f TDECL-FIELD-CLEANUP-XT and TDECL-FIELD-RELEASE-XT (:2014, :2023) are defers filled in the same file (:2145-2146) only so decl-event.f (:510, :537) can reach two private words; generated-declaration-protection.f (:115-116) undefines them after. A hook stands in for privacy (docs/design.md 'Machinery being replaced'). Acceptance: decl-event.f calls the two words plainly, they are unreachable from a program by the build's name strip, and the defers and undefine lines go. Depends: habu-re-enable-hb-a2404c26. Claim: unassigned.

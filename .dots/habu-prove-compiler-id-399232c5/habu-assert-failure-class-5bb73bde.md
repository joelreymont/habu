---
title: Assert failure class in Fail obligations
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T16:00:36.061979+02:00"
---

Full context: destruction review finding 3, MEDIUM. Rocq's 'Fail cmd' succeeds when cmd fails FOR ANY REASON, including an unbound reference. Renaming source_local to src_local consistently inside formal/Common/Ids.v makes the generated 'Fail Definition wrong_family_local := Ids.source_local (Ids.MkFunId ...)' pass on 'unbound reference' instead of on the family mismatch - the model then has no local projection for source ids at all while Habu's SOURCE-LOCAL is unchanged, and the parity gate stays exit 0. All five wrong-family twins currently fail for genuine type-mismatch reasons (verified individually), but nothing keeps that true. Required result: the generated wrong-family obligations assert the failure MESSAGE CLASS, not merely that the command failed - Rocq supports 'Fail' output capture via redirecting the error or by using an explicit unification test instead; choose a mechanism where an unbound identifier is distinguishable from a type mismatch and defend it. Acceptance: the rename mutation above fails the gate; the five twins still pass; gate green unmutated.

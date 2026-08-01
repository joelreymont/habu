---
title: Scope a locals group inside a control structure
status: open
priority: 3
issue-type: task
created-at: "2026-08-01T13:20:42.107761+02:00"
---

src/compiler/native/elaborate.f DO-CLOSE-LOCALS refuses a {: ... :} group that closes while a control structure is open (CS-N nonzero): names bound on one arm of a branch do not dominate the code after the join, and the elaborator has no scoping rule for that. The refusal is E-NELAB-LOCAL and test/compiler/native-elaborate.f NESTED-CASE pins it. Wanted: the checker's own scoping rule read off src/core/checker.f (every closer restores the locals mark, formal/Common/Control.v records it), then the same rule in the elaborator - a group opened inside a structure goes out of scope at its closer, and a name that outlives its structure travels as a block argument like every other live value. No corpus word needs it: LERP, BYTE-SUM and BYTE-FIND all declare at the top of the body.

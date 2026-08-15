---
title: Guard patch32 against an open band
status: open
priority: 2
issue-type: task
created-at: "2026-08-15T14:08:04.280611+02:00"
---

MAJOR from the bake-spine destruction review (2026-08-15): habu1.f's stateless patch32 pair (LPROTREC close leg) flips the two pages around the target to RX UNCONDITIONALLY - if the target lies inside the open code band (the normal case: patching an instruction just emitted mid-definition from an immediate word), the band still records those pages RW, the next LCEMIT sees CP in-range and skips the syscall, and the store dies exit 134. Fail-loud, and the pre-landing code had an equivalent hazard - but the landing's prose claims the pair is symmetric-safe and the invariant is NOT established (habu1.f:3347's own rule is violated in the scenario the comment invokes). Fix: the close leg skips or re-widens when the target pages intersect an open band, or prove patch32-inside-bracket unreachable and pin THAT. Files: src/habu/habu1.f. Depends: none.

---
title: Bind a wide arity-0 layout value to a typed local
status: active
priority: 1
issue-type: task
created-at: "2026-09-19T18:20:04.414478+03:00"
---

Problem: src/core/checker.f LOCAL-TYPE (~3951) refuses a typed local whose family layout is wider than one cell ('pt T-WIDTH 1 <> IF a u BAD-LOC-ANN'), so a two-cell domain value (a STRUCTURE with two fields, a span, remotefile in Tender) cannot stay a named local and code destructures and rebuilds it around every use. The binder already validates a hidden top term against the incoming bundle and reloads every physical cell on reference (MK-HIDDEN, the W>1 layout machinery), so the change is the front-end restriction only: record the layout's top hidden term ('pt dup T-WIDTH 1 - MK-HIDDEN') instead of refusing (the handoff of 2026-09-19, section 3, measured on the 2026-09-13 baseline: experiment/wide-typed-locals@origin commits 6d9681c5, ca54cdcf, 633c3e7f - read them as a reference, re-derive on this line, do not merge). Acceptance: ': ID ( pair -- pair ) {: p:pair :} p ;' over a two-field STRUCTURE certifies at tier 0 and tier 1 and returns the same two cells; a reference in a branch and a reference after a loop reload the whole bundle; the interpret-mode layout guard is unchanged; a wide local of a family with arity > 0 stays refused until the parametric-annotation dot lands; red-first fixture (a probe like test/wide-typed-local-probe.f from that branch, and the type-layout suite that owns W>1 locals); docs/forth.md locals section states that a layout local holds the whole value. Files: src/core/checker.f, test/, docs/forth.md. Verify: the fixture; three generations with cmp; test/run.f. Depends: none. Ownership: checker. Claim: agent=hazel workspace=.jj-ws/hazel-wide-locals

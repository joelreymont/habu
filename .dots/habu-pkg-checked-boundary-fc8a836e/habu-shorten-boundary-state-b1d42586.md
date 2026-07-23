---
title: Shorten boundary state
status: open
priority: 2
issue-type: task
created-at: "2026-07-23T03:48:23.448601+02:00"
blocks:
  - habu-own-boundary-lint-0cff8730
---

Why: after API ownership lands, the private constants, storage, pointer accessors, boolean helpers, and output helpers still carry the obsolete UB prefix. Exact result: in tools/checked-boundary-lint-core.f declarations currently spanning the constants and storage through UB-U$, drop only the UB- prefix from every private declaration and all of its references. The six public API words JSON!, STRICT!, OUT-FD!, RESET, FILE, and FINISH remain unchanged. Preserve every numeric capacity and character value, storage width, pointer field type, lazy or borrowed lifetime, write error, output byte, and number rendering behavior. Resolve names in package CHECKED-BOUNDARY-LINT only; do not export or alias any renamed word. Forbidden: semantic edits, reordered storage, merged state, magic replacements, wrappers, or changes outside this naming concern. Pre-change proof: the owned package still contains UB-prefixed private state names. Acceptance: no UB-prefixed declaration from this exact state/output slice remains, no public surface changes, the real command and CBLT suite are byte-identical, private/legacy probes reject, and exact ownership/type checks pass. Files: tools/checked-boundary-lint-core.f. Depends: habu-own-boundary-lint-0cff8730. Ownership: CHECKED-BOUNDARY-LINT private state and output names only. Claim: unassigned.

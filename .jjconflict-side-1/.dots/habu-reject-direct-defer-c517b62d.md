---
title: Reject direct defer self-install
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T23:16:26.608536+02:00"
---

Non-critical compiler dot from a measured SIGSEGV (2026-07-26, meminj lane 12-line reproducer). Exact invariant: installing a quotation as a defer target must be REJECTED when that quotation directly calls the same defer - direct self-install has no base path and can only recurse. Production negative: inside a package, declare private defer MAP, then [: map ;] is MAP - case-insensitive package lookup binds the body to the defer itself; the checker/compiler must reject before execution with a named diagnostic. Scope: DIRECT defer self-install detection only, in the checker/compiler; legal package shadowing is preserved in general (indirect recursion through another word is out of scope). Fixtures: the minimal negative regression above plus a positive control installing a distinct implementation quotation, both through the real load path. Owner: checker/compiler defer-install rule. Dependencies: none. Priority: non-critical optimization tree - no inference lane waits on it. Real pre-change failure: the reproducer compiles clean today and dies SIGSEGV rc 139 at first call.

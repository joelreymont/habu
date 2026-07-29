---
title: Retire the CHECK-HOOK name-suffix exemption
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T23:57:33.260833+02:00"
---

Full context: found by agent hookpath 2026-07-30 while retiring the name-only hook allowlist. UB-HOOK-NAME? in tools/checked-boundary-lint-core.f (~line 331) exempts ANY word whose name ends in CHECK-HOOK, in ANY file, from the UNCHECKED-DEFINITION finding while the checker is off. Same weakness class as the retired allowlist - a name-only rule where the (file, name) authority exists - but a different rule with different callers. Drive it from the tools/hook-sites.f registry the same way UB-HOOK-ALLOWED? now is (the pair predicate HOOK-SITES:CHECK-MATCH? is exported), with hostile fixtures: a word named MY-CHECK-HOOK in an unregistered file must be a finding; the registered sites stay green; mutation restoring the suffix rule reds the suite. Same test conventions as commit 19cec81e (Drive hook allowlist from the hook-sites table).

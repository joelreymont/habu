---
title: Nest generated family namespaces
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T16:16:01.754614+02:00"
---

Why (Joel, 2026-07-30): generated constructors mint mangled flat namespaces (MDLCFG-CFGKEY:MAKE, SAFET-MAP--TAKE:MOVED) that collide visually with real packages; type rendering already prints the nested form (maki:dtype<>). Result: qualified lookup learns a two-colon split (package, then child namespace, then word); namespace records gain a parent link (writers are exactly the sinks censused in the owner-authority round); the family definer creates a child namespace under the declaring package instead of a mangled flat name. Target spelling MDLCFG:CFGKEY:MAKE. NOT source-level nested package blocks - the flat package scope stays. Owner: engine half (codex). The migration sweep respells every reference. Acceptance: old mangled spellings unresolvable; new nested spellings resolve; words agree with type rendering; fixpoint passes at the conversion's single gate.

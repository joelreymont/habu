---
title: Nest generated family namespaces
status: closed
priority: 1
issue-type: task
created-at: "2026-07-30T16:16:01.754614+02:00"
closed-at: "2026-11-04T11:48:49.879149+02:00"
close-reason: "Census 2026-11-04, decided by Joel (2026-11-03): no STRUCTURE/ENUM family gets a namespace; the nested-namespace plan is reversed"
---

Why (Joel, 2026-07-30): generated constructors mint mangled flat namespaces (GPT2-CONFIG:MAKE, SAFET-MAP--TAKE:MOVED) that collide visually with real packages; type rendering already prints the nested form (maki:dtype<>). Result: qualified lookup learns a two-colon split (package, then child namespace, then word); namespace records gain a parent link (writers are exactly the sinks censused in the owner-authority round); the family definer creates a child namespace under the declaring package instead of a mangled flat name. Target spelling GPT2:CONFIG:MAKE. NOT source-level nested package blocks - the flat package scope stays. Owner: engine half (codex). The migration sweep respells every reference. Acceptance: old mangled spellings unresolvable; new nested spellings resolve; words agree with type rendering; fixpoint passes at the conversion's single gate.

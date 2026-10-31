---
title: Census self-restoring owners for throw-path rests
status: closed
priority: 2
issue-type: task
created-at: "2026-10-29T11:06:25.122747+02:00"
closed-at: "2026-10-31T20:52:55.260636+02:00"
close-reason: "Obsolete after lane 11: OWNERS-GATHER survives only in a comment; objects hold no owner state; a warm load re-runs the throwing top level."
---

tools/native-unit-object.f OWNERS-GATHER (~2898-2908) neither refuses nor carries an owner span that 'restores itself'. The cache cannot see a throw caught inside a judged top-level run (the run consumed source, no ticket opens), so an owner that restores only on success leaves dirty bytes in a reused file. FFI-DECL had this (re-review of onnsrluu: ' VERSIONED-LIBRARY catch and ' FUNCTION: catch, warm != cold; fixed by fold-in 5). List every self-restoring owner, check each one's throw paths with a ' WORD catch probe whose warm and cold products are compared (/tmp/rev-ffi4/probe/p6.f shape), and dot each owner that leaks. Claim: unassigned.

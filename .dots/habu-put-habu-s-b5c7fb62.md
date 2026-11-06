---
title: "Put Habu's global library words in packages"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T16:19:39.643658+02:00"
---

Problem: Habu AGENTS.md says every module has a real package, yet 10 library files define 177 words outside any package (count of `: NAME` definitions outside any package...;package at master 72a57d9e, test libraries not counted): process-env.f 53, string.f 50, float.f 20, process-argv.f 15, memory.f 13, ffi-abi.f 10, prelude.f 9, ffi-callback.f 4, span.f 2, type/deftype.f 1. A global name is taken from every program that loads the file (process.f's global outcome type kept Tender from naming its package OUTCOME). Joel 2026-11-06: 'those should be packaged but file a dot for this, don't do it now. just do process.f for now'. option<a> and result<a,b> (src/lib/adt) stay global: Joel 'these should be global, prove me wrong!', not disproved. Scale, uses outside the defining file (habu/zen/tender): string.f 8999/3525/4639, prelude.f 7305/3036/8379, ffi-abi.f 1418/208/3, process-argv.f 1009/68/62, process-env.f 500/21/23; maki, loom, etch and kiba also call them. Acceptance: every word those files define is in a package (public only where used outside it), Habu's own callers moved in the same change, clients told to migrate on install. Files: the ten above and their callers. Verify: no `: NAME` definition outside a package in those files; Habu's suites for the moved files. Depends: process.f's packaging (kevin, 2026-11-06). Ownership: Habu library modules. Claim: unassigned.

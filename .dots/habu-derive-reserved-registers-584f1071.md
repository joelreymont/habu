---
title: Derive reserved registers from the layout owner
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T10:36:19.538482+02:00"
---

CG-13. src/compiler/a64-effect.f:261-273 excludes only x18/x19/x30/x31, but engine state also occupies x20/x26/x27/x28 (src/habu/layout.f:3-6,191). NABI:POOL accepts those registers; a real select-allocate-emit-republish probe with base x20 corrupted the DATA/RBASE value and SIGSEGV'd (exit 134). Fix: one authoritative target/engine reserved-register set derived from the layout owner, enforced through every GPR constructor, set, sequence, pool, and allocation path; refuse before emission; never duplicate the set per pass.

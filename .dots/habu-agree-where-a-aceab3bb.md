---
title: "Agree where a signature starts: engine, checker, T0"
status: open
priority: 3
issue-type: task
created-at: "2026-10-29T04:44:59.307105+02:00"
---

The engine starts a signature at the first non-blank ( byte after the name (habu2.f C-SIG-START; definers.f:286-292); the checker needs ( followed by a space with only the name read (checker.f ~25261-25275); T0 needs a whole ( token (t0.f SKIP-HEAD). ': Q () 7 ;', ': Q (n -- n) 1+ ;', ( followed by a tab or newline: T0 and tier 1 refuse identically (undefined ()/(n, bad stored signature), the JIT accepts most. Nothing miscompiles; decide one rule (probably the checker's) and make the engine's reader refuse the rest. Probes /tmp/jerry-chk/rev3/tree/p/. Workspace: none yet.

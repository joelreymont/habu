---
title: "Agree where a signature starts: engine, checker, T0"
status: open
priority: 3
issue-type: task
created-at: "2026-10-29T04:44:59.307105+02:00"
---

Problem: the engine, the checker and T0 disagree on where a signature starts. The engine starts it at the first non-blank ( byte after the name, whole token or not (src/habu/interpreter.f:1553 DEF-SIG-START); the checker needs ( followed by a space with only the name read; T0 needs a whole ( token. For ': Q () 7 ;', ': Q (n -- n) 1+ ;' and ( followed by a tab or newline, T0 and tier 1 refuse identically (undefined ()/(n, bad stored signature) and the JIT accepts most. Nothing miscompiles. Decide one rule (probably the checker's) and make the engine's reader refuse the rest. It is a disagreement between two implementations of one thing: close it by removing the second implementation, not by patching that copy. Workspace: none yet.

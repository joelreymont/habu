---
title: Delete the JIT
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T20:55:09.916986+02:00"
---

Roadmap step 4, after T0 is the engine's compiler (step 3 = this dot's parent: bake T0 into the engine image, dispatch : to it, stop loading it from source in tests, rename to COMP per habu-name-the-build-abed309a). Open design question, Joel decides after a census: the startup files loaded before the checker exists are compiled by the JIT today; T0 compiles from the checker's notes and cannot compile them; on a cold start (hb-stdin, every build after B1's cold load) something must. Census first: which files precede the checker, what they contain, what the interpreter can run. Proof: cold start without the JIT, full suite, Gforth recovery check.

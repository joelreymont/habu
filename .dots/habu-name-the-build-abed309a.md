---
title: Name the build BUILD and the compilers COMP and XCOMP
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T20:41:13.769625+02:00"
---

Joel 2026-10-29: no NATIVE- or N- prefixes; there is no non-native build. The build is BUILD (the one-process hb-build merges NATIVE-BUILD-* and src/lib/build.f's BUILD; support-image packages are deleted, not renamed). The compiler is COMP (compile for the running machine) and XCOMP (cross-compile: another target or an engine's window). Apply each name in the rewrite that owns the code: the one-process build lanes (dot habu-build-a-tree-d00185b1) for BUILD; T0's Step 3 (bake T0, default, delete JIT) for COMP/XCOMP, with tier 1's N* packages going in Step 5. Add the rule to docs/forth-card.md §1 when the first rename lands.

Joel 2026-10-29 (late): the whitebox setter is BUILD:ENGINE-UNSEAL; COMP for the compiler and XCOMP for the cross-compiler confirmed. BUILD is the whole product job (cold-load, target window, seal, capture, smoke, install, settings); COMP and XCOMP compile one definition and read no build setting.

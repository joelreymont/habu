---
title: Decide what XCOMP names now that COMP holds every target
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T20:41:13.769625+02:00"
---

Source, Joel: "no NATIVE- or N- prefixes; there is no non-native build. The build is BUILD (the one-process hb-build merges NATIVE-BUILD-* and src/lib/build.f's BUILD; support-image packages are deleted, not renamed). The compiler is COMP (compile for the running machine) and XCOMP (cross-compile: another target or an engine's window)." And later: "the whitebox setter is BUILD:ENGINE-UNSEAL; COMP for the compiler and XCOMP for the cross-compiler confirmed. BUILD is the whole product job (cold-load, target window, seal, capture, smoke, install, settings); COMP and XCOMP compile one definition and read no build setting."

Problem: XCOMP names nothing, and every target's emitter is now a part of COMP: its X86-64 part (src/habu/t0-x64.f:1-2), its WASM part (src/habu/t0-wasm.f:1) and its ARM64 part (src/habu/t0-arm64.f:1), each chosen by CTARGET-ARCH (t0-x64.f:4-7). So cross-compiling is COMP with another target selected, and nothing carries the "another target or an engine's window" split that XCOMP was to name. No whitebox setter exists either (no BUILD:ENGINE-UNSEAL or other unseal word in src/ or tools/). Whether XCOMP still names a package, a part of COMP, or nothing, and whether BUILD:ENGINE-UNSEAL still names a word, is Joel's call; this dot makes neither choice.

Acceptance: Joel's decision on XCOMP and on BUILD:ENGINE-UNSEAL is recorded in docs/xcomp.md (Build host, product and target) and, where it names new work, filed as its own dot.

Depends: none.

Claim: unassigned.

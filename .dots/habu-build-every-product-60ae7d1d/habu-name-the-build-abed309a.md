---
title: Decide what XCOMP names now that COMP holds every target
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T20:41:13.769625+02:00"
---

Source, Joel: "no NATIVE- or N- prefixes; there is no non-native build. The build is BUILD (the one-process hb-build merges NATIVE-BUILD-* and src/lib/build.f's BUILD; support-image packages are deleted, not renamed). The compiler is COMP (compile for the running machine) and XCOMP (cross-compile: another target or an engine's window)." And later: "the whitebox setter is BUILD:ENGINE-UNSEAL; COMP for the compiler and XCOMP for the cross-compiler confirmed. BUILD is the whole product job (cold-load, target window, seal, capture, smoke, install, settings); COMP and XCOMP compile one definition and read no build setting."

Problem: BUILD (src/lib/build.f:12) and COMP landed and no NATIVE-BUILD- name is left; the forth-card rule is habu-add-the-no-cba01136. XCOMP names nothing yet, and the tree has since put every target's emitter into COMP as a part: COMP's X86-64 part (src/habu/t0-x64.f:1-2, :20), its Wasm part (t0-wasm.f:41) and its arm64 part (t0-arm64.f:22), each chosen by CTARGET-ARCH (t0-x64.f:4-7). So cross-compiling today is COMP with another target selected, and nothing carries the "another target or an engine's window" split that XCOMP was to name. No whitebox setter exists either (no BUILD:ENGINE-UNSEAL or other unseal word in src/ or tools/). Whether XCOMP still names a package, a part of COMP, or nothing, and whether BUILD:ENGINE-UNSEAL still names a word, is Joel's call; this dot makes neither choice.

Acceptance: Joel's decision on XCOMP and on BUILD:ENGINE-UNSEAL is recorded in docs/xcomp.md (Build host, product and target) and, where it names new work, filed as its own dot.

Files: docs/xcomp.md.

Verify: the decision is in docs/xcomp.md.

Depends: none.

Ownership: unassigned. Claim: unassigned.

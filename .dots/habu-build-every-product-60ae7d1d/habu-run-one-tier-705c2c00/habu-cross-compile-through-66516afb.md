---
title: Cross-compile through T0 dual emission
status: closed
priority: 2
issue-type: task
created-at: "2026-10-28T14:52:51.618180+02:00"
closed-at: "2026-11-10T21:52:35.495573+02:00"
close-reason: "obsolete: C0-C2 landed (ef1f85cb, 10ecb68f); C3 and C4 target the deleted capture build; C3 is now Wasm step 2 (0d5dab47) and C4 is x86-64's (3ed6f45d)"
blocks:
  - habu-move-t0-into-16d2feab
---

Problem: the shadow/capture path needs the target machine's walk from the host's T0, and T0 compiles no shadow routine today. Joel 2026-11-02 (DECISIONS.md, Compiler and builds) approved the design: every pass seals unplaced, only the host pass places, before sealing; T0 keeps its own private SHADOW-TAKE; T0's five files become package COMP, the emitter seam's words named EMIT-*. Approved public changes: COMP:INSTALL replaces T0:INSTALL and every other public word of T0, T0-EMIT, T0-LABEL, T0-A64 and T0-X64 goes private; EMIT-PLACE and EMIT-SEAL are private COMP words. Any other public word needs Joel's approval through the interview.

Plan, one commit each (design at /tmp/jerry-big/t0dual-design.md):
- C0 Move T0 into COMP: habu-move-t0-into-16d2feab.
- C1 EMIT-PLACE before EMIT-SEAL (COMP's emitter seam and LOWER/FIXED-LOWER). Proof: candidate build; tier0-inline-prims, tier0-compare-branch, tier0-join, tier0-string-literals and the T0 callback row; no case flips.
- C2 T0 walks once per machine, shadow first, host second (private HOST-ARCH, SHADOW-ARCH, SHADOW-TAKE, SHADOWED, HOSTED; SHUT abandons the shadow on every exit; runtime-arm64.f and runtime-x64.f drop the install-time select); test/x86-64-link-records.f runs on T0, its set-tiers and the stray and callee modes go. Fails before, passes after; test/compiler/shadow.f stays green. Found writing its brief: COMP is a sealed engine package (`hb --load src/habu/t0-x64.f` exits 84, E-SEALED-PACKAGE), so runtime-arm64.f builds the x86-64 emitter into the arm64 engine; T0 has no Wasm emitter and Wasm tests define tier-0 words while a Wasm shadow is open (test/wasm/capture.f ARM-HOOK), so T0 shadows only for a machine it has an emitter for, the capture still refusing a reached routine-less record; the Wasm lane removes that case. The runtime files' install-time select already went in C0. Brief /tmp/jerry-big/t0c2-brief.md.
- C3 fixed, cast and does> shadows from T0, after habu-make-the-fixed-fe3899b7 and habu-give-t0-s-b23058c5 (the cast kind comes from that ENUM, not a new constant); aot-shadow-capture on T0.
- C4 the engine cross-builds through T0, after habu-reuse-app-files-51821a5e (the rule: the engine build does not move to T0 until carl's cache works at any tier). tools/native-emit-x64.f requires t0-x64.f; native-build-core.f's AT-TIER-1 and tier juggling go. Proof: x86 engine cross-built by T0 on arm64; test/run.f and tools/two-generation-build.f on joel@pulsar-ua-106; arm64 --same-host chain; the x86 native-shadow tests with the tier-1 pin dropped.
A Fable feature review of the combined diff precedes landing C1-C4.

Files: src/habu/t0*.f, runtime-arm64.f, runtime-x64.f, tools/native-build-core.f, tools/native-emit-x64.f, test/x86-64-link-records.f, test/aot-shadow-capture.f, docs/xcomp.md. Verify: per commit above. Depends: the blocks list. Ownership: those files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c1. C1 (ef1f85cb "Place T0's emission before sealing") and C2 (10ecb68f "Compile T0 once per machine") landed 2026-11-02 after a full arm64 suite (514 of 514) and the two-generation chain (gen 5 = gen 4). The Fable feature review accepted both. One review finding is not adopted: under a non-native arm64 shadow (only test/compiler/shadow.f opens one) T0 files arm64 routines that no reader consumes, where tier 1 refuses because its arm64 backend has no unplaced form; T0's form is valid, nothing in production opens such a shadow, and whether a reader consumes a machine's form is the capture's question, not the emitter's. Another finding, a fixed word under an x86 shadow without passes.f dying with an uncaught -6616, moved into C3's proof (/tmp/jerry-big/t0c3-brief.md). The twin regression (hb-build-repl-twin F20/F21: PLACED-AT kept the compiling process's code address into every saved image) is fixed inside C1: EMIT-SEAL takes the placement out before anything can throw. Its review accepted all claims; its one Low (EMIT-CB-STUB's local on the name line, card section 2) goes to the seam lane of habu-give-t0-s-b23058c5, which rewrites that word. C3 is next after that dot's lanes; its T0 SHADOW-FIXED takes the compiler lease as tier 1's does once that dot's lease lane lands.

Census 2026-11-04: the claim named a workspace or agent whose work is gone or landed; reset to open, Claim: unassigned.

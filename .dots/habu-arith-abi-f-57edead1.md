---
title: arith-abi.f re-registers E-DIV-ZERO for a load order nobody has shown
status: open
priority: 4
issue-type: task
created-at: "2026-11-10T15:58:46.532995+02:00"
---

Problem: src/habu/arith-abi.f:13 registers -6400, the code src/lib/error.f owns as E-DIV-ZERO, a second time; its header (:1-4) says this is the one form tools/error-code-lint.f admits and that test/prim-parity.f keeps the two equal. Its old reason, that habu1.f compiles before src/lib exists, is false for the arm64 build: tools/hb-build-core.f requires error.f (:15) before its STARTUP (:128) runs. Whether any chain that requires arith-abi.f (src/habu/habu1.f:29, required by src/arch/arm64/boot.f:32; src/habu/kernel-x64.f:23; src/compiler/native/emit-x64.f:22) still loads it before error.f is not shown. Joel: "i want all issues identified to be dotted and fixed". Hunt the load order first. Acceptance: either the load order that needs the pair is in arith-abi.f's comment as path:line, or the emitters read error.f's code and arith-abi.f, its lint admission and test/prim-parity.f's equality case go. habu1.f and kernel-x64.f are machinery being replaced: if the removal needs an edit there, it joins the replacing design and this dot records that. Depends: none. Claim: unassigned.

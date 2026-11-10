---
title: arith-abi.f re-registers E-DIV-ZERO for a load order nobody has shown
status: open
priority: 4
issue-type: task
created-at: "2026-11-10T15:58:46.532995+02:00"
---

Problem: src/habu/arith-abi.f registers the (code, name) pair src/lib/error.f owns as E-DIV-ZERO, the one form tools/error-code-lint.f admits. Its old reason, that habu1.f compiles before src/lib exists, is false: tools/hb-build-core.f STARTUP loads error.f before kernel-object.f, which loads habu1.f. Whether any chain that requires arith-abi.f (habu1.f :11, kernel-x64.f :23, src/compiler/native/emit-x64.f :22) still loads it before error.f is not shown. Found by the worker of habu-comments-name-a-0f8f9c1c. Source: Joel: "i want all issues identified to be dotted and fixed". Hunt first. Acceptance: either the load order that needs the pair is in arith-abi.f's comment as path:line, or the emitters read error.f's code and arith-abi.f, its lint admission and test/prim-parity.f's equality case go. habu1.f and kernel-x64.f are machinery being replaced: if the removal needs an edit there, it joins the replacing design and this dot records that. Files: src/habu/arith-abi.f. Verify: the hunt's load-order table. Depends: none. Ownership: src/habu/arith-abi.f. Claim: unassigned.

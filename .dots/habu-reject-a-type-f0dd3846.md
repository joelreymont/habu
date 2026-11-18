---
title: Reject a type too deep to walk
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T13:16:26.521106+01:00"
blocks:
  - habu-move-the-interpreter-d35780fa
---

Problem: 4097 nested quotation locals make the checker exit rc 76 with 'checker: term walk too deep (cyclic term)' (src/core/checker.f TWALK-DEEPER), though the type is finite and no cycle can reach the walks (every binding to a structure passes an occurs check). Cause: the checker's walks recurse on the file's 64 KiB unit stack and the main thread's 64 KiB return and loop stacks; TWALK-MAX-DEPTH and EFFECT-DEPTH-MAX (checker.f:3136-3142) stand in for that capacity. Once those stacks are reservations (the blocking dot's acceptance case), delete TWALK-MAX-DEPTH, TWALK-D, TWALK-RESET, TWALK-DEEPER, TWALK-SHALLOWER and their EXPORTs; EFFECT-DEPTH-MAX with every rejection built on it (the SIG-TYPE stop, TERM-DEPTH, ROWS-DEPTH, ROWS-FIT?, USIG-DEPTH-FITS?, SGBAD-UNFIT!, MD-EFFECT-DEPTH, structure-decl.f ITEMS-FULL and ITEM-ROOM); make render.f RBUF grow; and have tools/check-core.f name `ulimit -s` when the verifier ends on the machine-stack bound. Acceptance: the 4097-level program and one past 8192 levels check; a test in test/signature-nesting.f pins it and fails first; the tests pinning the deleted rejections (signature-nesting, split-str-deep, structure-decl-suite, engine-suite) follow. Depends: habu-move-the-interpreter-d35780fa. Claim: unassigned.

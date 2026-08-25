---
title: delete the Gforth-hosted code outside the recovery closure
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:47:07.255001+02:00"
---

Problem: bootstrap/cg/{asm-checked,cglocals,cgloop,cgquot,disasm-core,disasm,inspect,install,link,opt,regstack,stepper,walk}.fs, bootstrap/{examples,habu-cg,habu-repl,habu-tui}.fs, bootstrap/src/{repl,sig,tui}.fs - 2,086 lines unreachable from forth.fs's require closure, unnamed by tools/bootstrap.sh and test/nf.fs (rg 0); the remaining bootstrap/src checker (1,650 lines) is pulled in only by bootstrap/cg/asm.fs:9 'require ../habu.fs' to check its own recipes, which the native fixpoint certify pass already certifies. MEASURED 2026-08-22 (text-page lane): the require closure of bootstrap/cg/forth.fs is 41 files of the 61 .fs on disk under bootstrap/, bootstrap/cg/, bootstrap/src/ (bootstrap/ 2 of 6, cg/ 16 of 29, src/ 23 of 26); bootstrap/habu.fs and habu-lib.fs are in the closure but outside both cg/ and src/. Acceptance: the files outside the closure deleted (the 2,086 lines named above, re-measured against that closure); a ruling on the ../habu.fs require (drop it and delete bootstrap/src, or keep it with a named consumer); recovery emission byte-identical. Files: bootstrap/. Verify: recovery gate; bootstrap-mirror-lint. Depends: none. Ownership: bootstrap. Claim: unassigned.

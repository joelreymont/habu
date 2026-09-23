---
title: Cut the residue of the retired seed codegen
status: active
priority: 2
issue-type: task
created-at: "2026-09-16T15:04:25.975578+03:00"
---

Claim: agent=alder workspace=.jj-ws/alder-seed-residue, based on 45608866.
Removed the unused CG-PRIMS wordlist and its entire template module, the dead
control-stack reset, and the linker-only runtime routines. The live seed keeps
its own stack moves and the two register constants it needs. Both Gforth
fixtures, a private check-only bootstrap, and all nine registry rows that read
the touched source passed. The wide fixture's generated image is byte-identical
before and after (SHA-256 8b54b61fe4db1cca6c9e70bf316fc69a9b16787ef70e22e83901a5823167ff72).
Evidence: ~/.cache/habu/seed-residue/source-45608866/. Awaiting Hazel's review
and integration.

Problem: 'Compile every baked recovery fixture checked' deleted bootstrap/habu-cg.fs and its cluster (bootstrap/cg/install, link, walk, regstack, opt, cglocals, cgquot, cgloop .fs; CODEGEN-HOOK in bootstrap/src/forward.fs and colon.fs), a generator entry point broken since 2c6bab11 (2026-06-11). That left dead code in files that stay: bootstrap/cg/templ.fs CG-PRIMS wordlist and the alias block filling it (about lines 161-215, read only by the deleted walk.fs EMIT-PRIM; cutting it orphans P-DIV, P-MOD, C-IF and their neighbours in turn), bootstrap/cg/rt.fs DOT-LBL and EMIT-DOT (read only by the deleted link.fs), and header comments in templ.fs (:4,5,46,165), asm.fs:136, rt.fs:3, icode.fs:2 and src/core/util.f:2 that still name deleted files. Acceptance: every word in bootstrap/ has a live reader on the stage0 path (tools/bootstrap.sh) or is deleted, cascading until nothing is orphaned; every comment names only files that exist; gforth test/bootstrap-engine-stack.fs and test/bootstrap-wide-memory.fs green; the stage0 chain reaches 'bootstrap check OK'. Files: bootstrap/cg/templ.fs, bootstrap/cg/rt.fs, bootstrap/cg/asm.fs, bootstrap/cg/icode.fs, src/core/util.f (comment). Verify: the two gforth fixtures; tools/bootstrap.sh with HABU_ALLOW_BOOTSTRAP=1 HABU_BOOTSTRAP_CHECK_ONLY=1 HABU_TARGET=linux-aarch64. Depends: none once the seed lane lands. Ownership: bootstrap/cg/*. Source: audit-seed worker note 2026-09-16; do after the release integrates since every seed change costs a chain run.

---
title: "Give a build's wordlist ids back at its close"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T03:06:34.155102+02:00"
---

Problem: LINKER:WITH's MOVE-IN takes two fresh wordlists per build (src/habu/linker.f MOVE-IN) and a product's packages take more, but WIDN lies in the sealed friend band (src/habu/layout.f WIDN-CELL $30), so the close cannot lower it (test/linker.f CLOSE-LEFT?). prot-wid-add exits 84 above PROT-WID-MAX 8192 (layout.f:696-698). With about 507 wids per build of hb's startup list, about 15 such builds in one process would exhaust it (inferred by the seal design, /tmp/jerry-s5c/design-seal.md section 9; not measured). A long-lived hb that builds repeatedly (agent iteration, DECISIONS 'build cost') is the case. Acceptance: measure the wids one startup-list build takes once S5c loads the list; then either the build's wordlist ids come from a range the close gives back, or the bound is shown out of reach by the measurement. Files: src/habu/linker.f, src/habu/layout.f, src/habu/habu1.f BWORDLIST; others per the design. Verify: a test/linker.f case running builds past the bound in one process. Depends: S5c's load and the seal design (the protected-wid band moves to the image during a build). Ownership: jerry. Claim: unassigned.

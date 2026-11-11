---
title: "Primitive facts text comes from the kernel object's rows"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T10:34:54.953226+02:00"
---

Problem: privacy step 2d (privacy-design.md Design 1): src/habu/link-data.f PRIM-TEXT lays each primitive's facts text from the host's CHECK:PRIMITIVE-FACTS, and src/core/check-hook.f OWN-OR-ROWS returns the host record's text first; src/habu/linker.f HB-OWNED finds an owned primitive's text by name in the host's private wordlist, which a stripped host lacks. Acceptance: PRIM-TEXT and the open's private records render facts from the rows the build stated (2c); HB-OWNED and OWN-OR-ROWS's record-first arm are deleted. Files: src/habu/link-data.f, src/habu/linker.f, src/core/check-hook.f. Verify: ~/.cache/tmp/jerry-strip/of1.f is on no build path; EPPRIM: CHECKER-OVERLAY imm-mark builds; engine fixpoint; consumer measure. Depends: 2c. Ownership: the lane. Claim: unassigned (brief to write).

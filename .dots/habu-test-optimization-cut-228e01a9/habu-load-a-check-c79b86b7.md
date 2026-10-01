---
title: Load a check subject once, by path
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T04:12:40.675977+02:00"
---

Problem: tools/check-core.f:1197-1200 CHK-BUILD-RUN builds the run file by pasting the origin-marked subject text after its prefix instead of loading the subject by path, so a source the engine already holds (engine-baked) is evaluated a second time in the check run. Measured 2026-09-30 on d40cc36d: tools/check.f on src/habu/dialect.f stops rc 78 E-DUPLICATE-DEFINITION, and lib/num-types.f (duplicate E-NUM-NEGATIVE), lib/adt/option.f (duplicate error family 7102), and spill.f, regalloc.f, a64ir.f, select.f, dict.f refuse the same way while each loads rc 0. The diagnostic is wrong too: tools/check-all-errors-core.f:258-270 CA-JSON-DUP reports line 1 and word "duplicate-definition" instead of the duplicated name and its line. Overlaps viper's checker plan (/Users/joel/Work/zed-habu/docs/habu-checker.md, "The fix, in order" steps 1, 3, 4; ENGINE-PROVIDES?): agree the mechanism with viper before the lane starts. Acceptance: tools/check.f gives every listed source the verdict its real load path gives (rc 0 here); a real duplicate definition in a subject is still refused and its report names the word and its line; cases through tools/check-test-lib.f written before the code. Files: tools/check-core.f, tools/check-all-errors-core.f, tools/check-test-lib.f. Verify: tools/check-test.f, tools/check.f on each listed source. Depends: habu-let-check-f-f02aa703 (r4-render, plan 15). Ownership: how the check run loads its subject.

Owner: viper (zed-habu checker plan). Done in viper's change okztpovq a05a770170c2 (habu .jj-ws/lsp): CHK-BUILD-RUN loads the subject by its canonical path; a subject the engine already provides exits 64 "already provided". The located duplicate packet (CA-JSON-DUP) is viper's step 4. Not round-4 work.

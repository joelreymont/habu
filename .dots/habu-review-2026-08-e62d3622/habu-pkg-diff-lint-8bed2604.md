---
title: package-diff-lint is an allowlist engine
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:47:07.155329+02:00"
---

Problem: tools/package-diff-lint-core.f (2,009 lines, ~700 of prose) carries 12 exception categories / ~50 entries: GLOBAL-IMPLEMENTATION? 17 exact paths (:413-434), fixture rows (:614-622), stage0 (:1204), engine-trunk (:1376-1383), mirror, TFAM-BRIDGE? 6 names, TYPE-DECL-GRAMMAR? 7, GLOBAL-SURFACE? extras; the retirement dot named at :472 (habu-pkg-internal-word-da4149d9) does not exist; :1106-1114 is a spliced paragraph and :1213 repeats 'the third principled category'. Root cause: the rule 'every changed definition must be packaged' is enforced on a mostly unpackaged tree, so each blocked edit adds a row; ENGINE-BODY-EDIT? (:1392-1395) is already the structural rule for five files. Acceptance: OLD-GLOBAL? body-edit admission is the default for every file (new globals still report); GLOBAL-IMPLEMENTATION? and the trunk/mirror rows deleted; the prose cut to the rule; the phantom dot replaced; the lint's tests keep every forgery fixture. Files: tools/package-diff-lint-core.f, package-diff-lint-test.f. Verify: the lint's test; a packaging commit of one legacy file passes (habu-unpackaged-modules-frozen-780b5f31). Depends: none. Ownership: package lint. Claim: unassigned.

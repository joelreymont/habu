---
title: "Fold case in the clobber lint's word compares"
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T19:00:02.014493+01:00"
---

Problem: tools/lint/clobber-lint-core.f MN-KIND (:280), the MN-* lists and INSTR?'s UPPERISH? (:81, :209) compare spelling case-exactly while Habu folds case, so a lower-case instruction such as `bl,` is an unknown call to the analysis and a lower-case `Lname LABEL@ bl,` site is reported unmet. Acceptance: the lint classifies a word whatever its case; a lower-case site is met; the lint is clean on the tree; UNMET's text no longer says upper-case. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-lintcase.

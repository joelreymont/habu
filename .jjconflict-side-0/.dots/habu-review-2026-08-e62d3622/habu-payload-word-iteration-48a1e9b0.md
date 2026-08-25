---
title: payload-word iteration is written three times
status: open
priority: 3
issue-type: task
created-at: "2026-08-23T12:28:40.502663+02:00"
---

Problem: the rule 'a path is one whitespace word of a string literal payload' is implemented separately in tools/lint/schedule-lint.f (pre-existing), tools/maki-dep-lint-core.f (2026-08-23 port) and tools/stdin-closure-lint-core.f (same day) - the same duplication the launcher parse had before BOOTSTRAP-SRC. Also: bootstrap-mirror-lint.f keeps SEED-N/SEED-HAS? as one-line views over BOOTSTRAP-SRC only so its test stayed byte-identical as the migration's identity proof. Acceptance: one payload-word iterator in tools/lint (source-lex's CONTENT side) consumed by all three; bootstrap-mirror-lint's test pointed at BOOTSTRAP-SRC:ROWS/HAS? and the two views deleted; all four suites green. Files: tools/lint/source-lex.f or a sibling, the three lints, bootstrap-mirror-lint.f + test. Verify: the suites. Depends: habu-three-lints-still-eb2aceee (landing). Ownership: lints. Claim: unassigned.

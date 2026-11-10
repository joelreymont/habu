---
title: all-errors throws again a refusal the replay already reported
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T19:36:43.590410+02:00"
---

Problem: packages PA and PB each export a public immediate PW; then 'using PA', 'using PB', 's" PW" 0 parse-imm' (or 'export PW' on the engine before the parse-imm replay). The load is refused 70 with E-USING-AMBIGUOUS and tools/check.f --verify-only gives a placed packet, rc 70; --all-errors exits 67 with 'check.f: uncaught throw code 7144'. Cause as read by the review of nykylzmy (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/pimm-review.md F2, run on that lane's engine and its parent's; not rerun by the lead): tools/check-all-errors-core.f CA-RUN-COMPOSE-DEFS (:582-591) treats a refusal it has already reported as a raw failure and CA-RUN-COMPOSE throws it again with nothing to catch it. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: --all-errors reports the refusal once, placed, and exits 70 as --verify-only does, for every refusal the replay raises at a top-level statement. Files: tools/check-all-errors-core.f, a case in test/diag-position-test.f. Verify: the two-package file under --all-errors: one packet, rc 70. Depends: none. Ownership: the all-errors tool's handling of a replay refusal. Claim: unassigned.

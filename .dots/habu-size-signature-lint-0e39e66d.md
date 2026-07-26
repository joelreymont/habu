---
title: Size signature lint buffer dynamically
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T18:30:51.109296+02:00"
---

Second file past signature-lint's fixed 64K buffer (SL-FILE-CAP, tools/signature-lint-core.f:9), and this one CANNOT be split: refine-lint confines the N>CENSUS/N>TABLE erasures to maki/infer/gpt2-bind.f by exact path, so the module must stay one file (67020 bytes before the S6b4 lane, 69443 after; the lint hard-fails rc=1 'file exceeds buffer' and reads nothing). tools/lint/text.f already provides the runtime-sized LINT-SOURCE buffer - migrate signature-lint onto it, delete the fixed cap, hostile fixture proving a file just over the old cap now lints (and a genuinely absurd size still refuses with a named diagnostic rather than silent truncation). Also fix the stale refine-lint-core rows 60/61 comment (reached-only-by-ABORT is wrong: ABORT-CHECKED, ABORT-CHECKED-ALLOC, CA-PREP-BACK, CA-TBL-BACK, COMMIT-MAPPED, RELINQUISH all read them) in the same commit - comment truth, no behavior change. Acceptance: signature-lint green on gpt2-bind.f and all split suites; the lint's own suite green with the new fixtures; both diff lints. Owner: the signature-lint package. Dependencies: none.

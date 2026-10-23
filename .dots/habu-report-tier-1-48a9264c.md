---
title: "Report tier 1's refusal of a clause naming a head local"
status: active
priority: 3
issue-type: task
created-at: "2026-10-23T18:57:57.818592+03:00"
---

Problem: at ARM64 tier 1 a does> clause that names a local of its definer's head (~/.cache/tmp/jerry-t0fix-cases/c4b-clause-names-head-local.f, 2026-10-23 on 387d46d0) is refused rc 67 `uncaught throw code -8572` with no diagnostic. The refusal is right; the report is not. Acceptance: tier 1 prints the checker's diagnostic naming the word and returns the checked-refusal code tier 0 returns for the same program; a native-suite row. Claim: jerry, worker in .jj-ws.noindex/jerry-t1report.

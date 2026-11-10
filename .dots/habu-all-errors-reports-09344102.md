---
title: all-errors reports one of two refusals at evaluated tokens
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T18:24:27.425584+02:00"
---

Problem: with 'TRUSTED: EV ( -- ) s" drop" evaluate ; immediate' / 's" EV" 0 parse-imm' / ': P ( -- ) ( a comment here ) 1 drop EV ;' / ': Q ( -- ) 1 drop EV ;', 'tools/check.f -- --json-errors --all-errors' prints one packet (P) where the same two definitions with a plain 'drop' print two. Reproduced by the lead on master 951b3f90. Found by the re-review of habu-a-preflight-refusal-87f2d9f2 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/rows-rereview.md, Found outside the fold): the run stage stops at its first refusal and the all-errors replay does not see text an immediate evaluated. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Hunt first: which stage reports P, why Q is not reached, and what the replay is given. Acceptance: --all-errors reports every refused definition of the file, each once, whether its refused token is the file's or one an immediate evaluated. Files: tools/check-all-errors-core.f and what the hunt names. Verify: the reproducer in test/diag-position-test.f or the all-errors test the hunt names. Depends: none. Ownership: the all-errors replay of immediate-evaluated text. Claim: agent=bob workspace=.jj-ws.noindex/habu-all-errors-reports-09344102

Hunt 2026-11-10 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/allerrors-hunt.md): the replay applies an immediate's declared row and runs nothing, so the check tool falsely refuses ': ACCW ( x -- ) EV ;', which the load accepts; and the run stops at its first refusal. Part A (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/Defer-unrun-immediate-bodies.md, in work): the replay defers to the run a body that uses an immediate the engine has not run. Part B, after A: the run reports every refusal under --all-errors, and duplicates are removed in the parent from packet data, never by a new hook input; it waits on part A, on the silent multi-error count (habu-count-only-refusals-e8b5dd42) and on the places of habu-an-evaluate-inside-d3da5e06.

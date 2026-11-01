---
title: Refuse a replay that reaches a file discovery never harvested
status: open
priority: 2
issue-type: task
created-at: "2026-11-01T08:47:42.383611+02:00"
---

Problem: in tools/source-discovery.f, SD-R-CONTEXT-BETWEEN exits without a word when SD-FILE-FIND finds no harvested file for the path the replay reaches, so that file's declarations, retirements and parses: rows take no part in the context check: a silent fallback (disc worker, by reading; on master before lane disc). Acceptance: find whether the case can occur (a load the replay follows that the harvest walk skipped); if it can, refuse with an E-DISC code and add an event-closure-test case failing before; if it cannot, state the invariant at the check and make its failure a throw, not an exit. Files: tools/source-discovery.f, tools/event-closure-test.f. Depends: lane disc landed. Claim: unassigned.

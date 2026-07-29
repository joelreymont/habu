---
title: Delete resident and its erasures
status: active
priority: 2
issue-type: task
created-at: "2026-07-26T22:59:20.807806+02:00"
blocks:
  - habu-cut-gpt2-model-445a19ff
---

After the cutover: delete WSTORE:resident, HOLD, the park/unpark trusted erasures, the pre-reserved HOLD cells in every table block, RESIDENT-DISPOSE, and their tests and TRUSTED.md rows, after a boundary-aware caller sweep proves zero references. Close the now-obsolete habu-add-wstore-scoped-e57e32e2 (its WITH-RESIDENT-SLOT capability has no consumer once the resident is gone); amend habu-add-wstore-resident-2ada0262 to landed-history status so it does not read as an active future direction. Acceptance: sweep table (zero references per deleted word); weight-store and bind suites green; TRUSTED.md rows retired; both diff lints.

Claim: agent=claude workspace=.jj-ws/habu-delete-resident-and-05c594cb

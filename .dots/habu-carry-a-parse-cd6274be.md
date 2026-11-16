---
title: Carry a `parses:` row through EXPORT
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T15:43:01.439033+01:00"
---

Problem: EXPORT-META-COPY (src/core/checker.f, rg) copies no PROW row (rows are keyed by symbol, PROW-FIND), so an exported word with a `parses:` row may lose its row through the export; unconfirmed for `parses:` rows: probe first. Acceptance: a program calling a word with a `parses:` row through its export is accepted by the load and by tools/check.f alike, each reading its payload raw. Depends: none. Claim: unassigned.

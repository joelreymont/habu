---
title: Carry a parse-imm row through EXPORT
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T15:43:01.439033+01:00"
---

Problem: EXPORT-META-COPY (src/core/checker.f, rg) copies no PIMM row, so an exported parse-imm word is judged differently: --verify-only rc 0 while the load rejects E-UNMODELED-IMMEDIATE, and in the load the export loses its row. Acceptance: a program calling a parse-imm word through its export is accepted by the load and by --verify-only alike, each reading its payload raw. Depends: none. Claim: unassigned.

---
title: Trace the checker for quotations that read locals
status: active
priority: 2
issue-type: task
created-at: "2026-11-11T07:54:58.987799+02:00"
---

Read-only census for the approved design (DECISIONS.md 'quotations read locals'): the quotation type's parts, how the keeper flag is raised, saved and applied, where a scope ends, what else assumes no local in a quotation, the saved effect record, how a checker code is added. Brief: .jj-ws.noindex/jerry-wasm/.out/briefs/quot-census-checker.md. Acceptance: a report answering its six questions with path:line and the probes run; no tracked file changes. Verify: the report. Depends: none. Ownership: none (read-only). Claim: agent=jerry workspace=.jj-ws.noindex/quot-census-checker.

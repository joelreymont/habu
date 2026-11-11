---
title: Trace the checker for quotations that read locals
status: closed
priority: 2
issue-type: task
created-at: "2026-11-11T07:54:58.987799+02:00"
closed-at: "2026-11-11T08:11:14.450417+02:00"
close-reason: "census reported 2026-11-11: quotation type fields, the NSCOPE keeper flag on type and row variables, SCOPE-EXTERIOR?'s scan, the two refusals and the saved effect nodes traced with path:line; the call-site probe of a flagged variable did not reach the flag (answers b and d rest on reading); feeds the checker lane briefs"
---

Read-only census for the approved design (DECISIONS.md 'quotations read locals'): the quotation type's parts, how the keeper flag is raised, saved and applied, where a scope ends, what else assumes no local in a quotation, the saved effect record, how a checker code is added. Brief: .jj-ws.noindex/jerry-wasm/.out/briefs/quot-census-checker.md. Acceptance: a report answering its six questions with path:line and the probes run; no tracked file changes. Verify: the report. Depends: none. Ownership: none (read-only). Claim: agent=jerry workspace=.jj-ws.noindex/quot-census-checker.

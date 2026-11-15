---
title: "Lex escapes through the engine's escape reader"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T07:46:08.746903+01:00"
closed-at: "2026-11-16T04:07:35.577511+01:00"
---

Problem: --all-errors lexes an escaped literal with src/lib/source-lex.f SKIP-ESC-QUOTE (:272), which checks no escape, and writes its own string record (tools/check-all-errors-core.f CA-LEX-RECORD :185), so a bad escape before an unclosed string is reported at the later string in other words. Acceptance: all four readers print the load's record at the bad escape; the uncalled lex record writers are gone. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).

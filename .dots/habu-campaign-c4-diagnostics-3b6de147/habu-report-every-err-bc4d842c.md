---
title: Report every error under --all-errors
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:25:48.491847+01:00"
closed-at: "2026-11-16T04:07:34.254239+01:00"
---

Problem: the C4 review saw `--all-errors` print only E-RESERVED-DEFINITION for a source with others (reserved-name-lint-core.f:229), and for a source whose statements throw print no E-STATEMENT-THROW or E-EVAL-RESIDUE and exit 67; THREW? remains (check-all-errors-core.f:229, 268, 273). Acceptance: the cause found and fixed; each error of such a source prints one record and the run stops with 70. Depends: habu-make-the-checker-0955002d. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).

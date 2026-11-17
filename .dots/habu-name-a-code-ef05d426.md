---
title: "Name a code by ERROR:NAME$ alone"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T15:54:12.268837+01:00"
---

Problem: tools/lint/text.f:298-327 keeps a code-name table of its own (LINT-CODE-NAME+, filled by tools/lint/token.f:8 and tools/lint/intern.f:14, read by tools/lint/lib.f:45-49) for exit statuses 76 and 77, which are no ERROR:CODE rows; habu-name-each-exit-3fc49d37 gives each exit status one name in one package, outside ERROR. Acceptance: the lint table is deleted and lib.f names a status as that package does. Depends: habu-name-each-exit-3fc49d37. Claim: unassigned.

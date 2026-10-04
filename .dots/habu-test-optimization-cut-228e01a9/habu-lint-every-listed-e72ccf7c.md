---
title: Lint every listed file for the checked boundary
status: open
priority: 1
issue-type: task
created-at: "2026-10-02T14:50:44.739677+02:00"
---

Problem (fold 339 r4-dupdiag, 4597b025): tools/check-core.f CHK-RUN-BOUNDARY (just above CHK-RUN-RESERVED-NAMES) lints the generated 'required' stub in --source-list mode, not the listed files, so a listed file can switch the checker off unrefused: $HOME/.cache/tmp/kestrel-r4-dupdiag/f3/fx/bx.f ('0 set-check' then a definition) is rc 1 single-file and rc 0 under --source-list, before and after 4597b025. Acceptance: list mode runs the boundary lint on each listed file as CHK-RUN-RESERVED-NAMES now does (CHK-LINT-LISTED, skipping engine-provided files), bx.f refused in list mode with its location in prose, --json-errors and --all-errors; seen failing first; check/source-list-* cases pass. Files: tools/check-core.f, tools/check-test-lib.f.

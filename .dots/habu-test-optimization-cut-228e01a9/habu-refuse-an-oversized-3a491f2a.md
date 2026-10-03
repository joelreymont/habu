---
title: Refuse an oversized diagnostic buffer by name
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T12:02:57.315333+02:00"
---

Problem (lane 284 repcap): src/core/render.f:85 RDIAG-APPEND ends the process with '76 die' ('render: diagnostic buffer full') once raw diagnostics pass RDIAG-CAP (1 MiB): about 2500 refusals under tools/check.f --all-errors --json-errors. The REPL and --all-errors lose every record with no rollback (docs/forth.md: interactive paths recover by throw). Acceptance: the overflow becomes a named throw the hook reports, or the buffer grows to what the run produces; a fixture with enough refusals to pass 1 MiB shows a diagnostic and a nonzero exit (or every record), not rc 76. Files: src/core/render.f, lib/errors.f. Baked: rebuild bin/hb. Note (review 301): RDIAG-CAP is not a render.f constant; DIAG-BUFFER! (src/core/render.f:56-59) installs the scratch each CLI hands the core (check.f CHK-RUN-CAP, check-all-errors.f ERR-CAP, both INCLUDE-BUF-CAP = 1 MiB), so the fix may live in the CLIs' scratch or in render.f's append.

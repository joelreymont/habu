---
title: Refuse a diagnostic record past the render buffer by name
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T14:28:09.638799+02:00"
---

Problem (review 342 of d781bcfd): src/core/render.f:25 EMIT-RAW dies 76 'render: sig buffer full' when one diagnostic record overflows RSBUF (16 KiB, render.f's own per-record buffer), e.g. a refused definition with a 20 KB name, in-process (REPL, --all-errors lose the session). Acceptance: the overflow throws a named code the caller can catch (as RDIAG-APPEND now throws E-DIAG-CAPACITY), or the record is truncated with a stated marker; a fixture through tools/check.f --all-errors --json-errors shows a diagnostic and check.f's documented exit, never 76; seen failing first. Files: src/core/render.f (baked), a test beside test/diag-buffer-capacity.f.

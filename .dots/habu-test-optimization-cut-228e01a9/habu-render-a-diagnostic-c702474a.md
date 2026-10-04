---
title: Render a diagnostic record past the 16 KB buffer
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T21:11:53.775445+03:00"
---

Found by 490 run 6 (4d fold, jerry-batch4d): a diagnostic record larger than src/core/render.f:13 RSBUF-CAP (16 KB) is refused -2901 E-DIAG-CAPACITY from EMIT-RAW (render.f:40-43), and `check.f --json-errors --all-errors` then exits 67 and prints a line that is not JSON. Seen on the 4d fold engine with a 8191-output stored signature (tdeep8191.f) and on the pre-fold g1 with a 16 KB unparseable signature ($HOME/.cache/tmp/kestrel-r4-b4dfold/fxb/bigsyntax.f). Acceptance: a record past the buffer either renders whole or is refused by a named, bounded record (the JSON stream stays valid JSON, one record per line) with the status a rendered refusal has (70 at --load and check.f); seen failing first through the real load path in default, --all-errors and --json-errors modes; no fixed second buffer size. Base: after 4d lands (render.f, diag-code.f). Baked: rebuild, g1 == g2 with .names, two-generation build.

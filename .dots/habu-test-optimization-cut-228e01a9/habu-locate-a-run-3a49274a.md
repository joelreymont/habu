---
title: Locate a run refusal in a multi-line definition
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T11:02:08.026802+02:00"
---

Problem (r4-originmark, bd62bc07): a definition that spans lines and is refused by check.f's run reports the wrong place: $HOME/.cache/tmp/kestrel-r4-originmark/run/r2.f has its token at line 4 column 4 but the record says line 3 column 18, byte 103. src/core/render.f JLOC-CALC/JABS-LINE/JABS-COL/JABS-BSTART (~:815-829) count from the definition's text as rendered into definition_source, not from its file span, so DIAGL0/DIAGC0 plus the offset in the joined text misses every line break the rendering dropped. Same with the old rewriter. Acceptance: a run-refused definition spanning several lines (token on line 2+, after a comment line, after a long first line) reports the token's own file line, column and bytes in prose and --json-errors; one-line definitions unchanged; cases through tools/check-test-lib.f seen to fail first; engine rebuilt (render.f is baked), g1 == g2, two-gen. Files: src/core/render.f (or where definition_source is built), tools/check-test-lib.f. Base: after the master merge (render.f is master-changed).

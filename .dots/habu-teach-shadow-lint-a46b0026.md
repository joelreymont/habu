---
title: Teach shadow-lint to skip string literal bodies
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T01:08:31.455935+02:00"
---

Lint tokenizer gap found during the RSEXEC flip (038476cd): tools/lint/shadow-lint.f tokenizes s" ... " string BODIES, so a definer keyword inside prose - e.g. a diagnostic suggestion containing 'variable or create' - is read as 'variable' defining the next token ('or'), producing a false shadow finding against a core prim. The worker had to reword render.f suggestion prose to dodge it. Fix: make the shadow-lint tokenizer consume s" / s\" / ." string bodies as opaque spans (matching the real parser), add a regression fixture with a definer keyword inside a string literal that must produce 0 findings, and restore natural prose in the render.f suggestion if desired. Files: tools/lint/shadow-lint.f, its fixture/test, optionally src/core/render.f.

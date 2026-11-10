---
title: FIELD= ends the whole test run on a missing JSON field
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T11:14:29.170477+02:00"
---

Problem: in test/diag-position-test.f, FIELD= on a packet that lacks the field stops the run with `missing JSON field` instead of recording one failure, so the cases after it never run and one regression hides the rest. Seen on the base engine: the first new locals-group case fails and the other three never run. Found by the locals-group fold-in worker. Source: Joel, 2026-11-10: "you must dot and launch a fix automatically for any issues you identify". Acceptance: a missing field is one failed assertion with the field's name, and the run goes on to its report. Files: test/diag-position-test.f (FIELD= and the reader it calls). Verify: the file on bin/hb; a copy with one asserted field renamed reports one failure and still ends at its report. Depends: habu-a-locals-group-0c34ca08 landed (same file). Ownership: the diagnostic tests. Claim: unassigned.

Claim: agent=bob workspace=.jj-ws.noindex/habu-field-ends-the-614c2257

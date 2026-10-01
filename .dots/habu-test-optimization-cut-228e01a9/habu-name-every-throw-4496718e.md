---
title: "Name every throw two-gen's MAIN ends"
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T07:23:23.238818+02:00"
---

Problem: tools/two-generation-core.f MAIN (:438) ends every throw other than its own verdicts with an empty-message die, so a throw from TG-MKDIRS, TG-HOST0, TG-BYTE-DIFF or any lookup exits 67 with nothing on stderr and the operator cannot tell what failed (found by the r4-small reviser after 2641c8f1 fixed the same-host verdict order). Acceptance: every throw MAIN catches prints a line naming the step or the code before the run exits, keeping TG-FAIL-RC for a verdict; a forced throw from a step in a scratch copy shows the line, before and after. Files: tools/two-generation-core.f. Verify: tools/two-generation-test.f, the forced run. Ownership: two-gen exit messages.

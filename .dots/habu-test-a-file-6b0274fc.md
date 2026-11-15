---
title: Test a file-system code in one word
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T00:04:02.272767+01:00"
---

Problem: the range test rc E-FS-FIRST <= rc E-FS-LAST >= and is written four times: tools/check-verify-core.f CHK-FS-RC?, tools/source-discovery.f HARVEST and READ-THROW, tools/lsp-text.f FS-RC?. Acceptance: one word beside E-FS-FIRST in src/lib/error.f answers it and all four call it. Depends: habu-campaign-c4-diagnostics-3b6de147. Claim: unassigned.

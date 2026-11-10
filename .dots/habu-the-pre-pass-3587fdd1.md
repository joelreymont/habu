---
title: The pre-pass accepts an unmodeled immediate the load refuses
status: active
priority: 1
issue-type: task
created-at: "2026-11-10T15:15:38.271933+02:00"
---

Problem: 'bin/hb --load tools/check.f -- --verify-only' exits 0 with no packet for /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/prose-review/p/imm.f, which --load and --json-errors refuse with E-UNMODELED-IMMEDIATE; docs/forth.md :2099-2100 says otherwise. The engine before habu-a-load-s-99fed2b4 does the same. Found by its review (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/prose-review.md, separate issue c). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a hunt names as path:line why --verify-only passes the definition; then the three modes agree on imm.f and a test pins it. Files: src/habu/verify-source.f, tools/check-verify-core.f, test/diag-position-test.f. Verify: imm.f in the three modes. Depends: none. Ownership: the source pre-pass. Claim: agent=bob workspace=.jj-ws.noindex/habu-the-pre-pass-3587fdd1

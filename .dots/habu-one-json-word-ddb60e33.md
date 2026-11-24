---
title: "One JSON word says whether a reader code is the input's fault"
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T22:24:44.371563+01:00"
---

Problem: the test 'a JSON reader code the input raised' (E-JSON-READ-FIRST..LAST, less E-JSON-READ-STATE and JSON:E-READ-SOURCE, which no input raises) is copied in src/lib/net/bearer.f MALFORMED? (:171), src/lib/net/test-issuer.f, src/lib/net/model.f, src/lib/net/mcp.f, tools/lsp-diag.f and src/lib/secret/azure.f; some copies miss the two exclusions and turn a reader bug into the input's fault. Acceptance: one public JSON word answers it; every copy calls it and is deleted; each caller's tests pass. Depends: habu-read-secrets-from-4d669ccc. Claim: unassigned.

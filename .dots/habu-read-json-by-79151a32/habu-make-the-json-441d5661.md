---
title: Make the JSON token reader private
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T17:22:27.523891+01:00"
blocks:
  - habu-walk-a-json-da90ebe1
  - habu-compile-a-json-92f9f4cf
  - habu-validate-a-json-9ec8e7c9
  - habu-read-mcp-requests-4c03d74e
  - habu-answer-mcp-client-b40eb00f
  - habu-read-odata-responses-fb2c90dc
  - habu-read-json-by-a447b521
---

Problem: read.f's reader stays public while anything outside JSON uses it. Acceptance: every READ-*, T-*, KIND$, JSON:reader and JSON:cursor is private or deleted, with private words left with no caller (INT-INNER, U64-*, FLOAT-INNER, the LISTED family, REPEAT-IN/-WITH, LOCATE/-WITH, MINT-READER/ERASE-READER); json-read-test.f and json-pointer-test.f read through PARSE and value words; json.f and json-rpc.f headers rewritten. Five words test the reader's code range by hand (bearer.f and test-issuer.f MALFORMED?, json-schema.f:200, model.f:1757, mcp.f:979). Depends: every other child. Claim: unassigned.

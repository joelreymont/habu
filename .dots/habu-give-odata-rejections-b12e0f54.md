---
title: "Give OData rejections the readers' rejection text"
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T00:16:43.043926+01:00"
---

Problem: a rejected $metadata document or response body writes one odata log line with a byte offset, what was expected and what was found, then throws E-METADATA or E-RESPONSE with no REASON text (src/lib/odata/response.f LINE-AT and REJECT, src/lib/odata/metadata.f's rejections, src/lib/odata.f header); the JSON and XML readers now throw with REASON text `L:C: expected X, found Y; fix`, so a caller of ODATA gets neither the place as line and column nor a fix, and reads it from a different channel than for JSON or XML. Acceptance: E-METADATA and E-RESPONSE carry REASON text `L:C: expected X, found Y; fix`, L:C counted in the document, the body, or for COPY-ENTITY the record; a JSON or XML reader rejection inside them throws on with the reader's own text; no odata log line; the odata tests check the text of each rejection kind. Depends: none. Claim: unassigned.

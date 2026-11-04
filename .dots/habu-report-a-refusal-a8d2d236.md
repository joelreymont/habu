---
title: Report a refusal PQconnectStart detects with its text
status: open
priority: 2
issue-type: task
created-at: "2026-11-04T11:36:46.324280+02:00"
---

A refusal libpq detects inside PQconnectStart (hosts whose Unix-socket directory is missing) surfaces as an uncaught E-CONNECT -9250 with no text, rc 67 (code review of kmpzlyvm). Inference: PQsetnonblocking refuses a connection already bad, so pg.f:1311-1313 takes its E-CONNECT branch before CONNECT-REFUSED keeps the text. Acceptance: such a connect answers refused with libpq's text, as any other refusal; regression row in src/lib/pg-test.f. Related: habu-close-the-pg-27b3f4d3.

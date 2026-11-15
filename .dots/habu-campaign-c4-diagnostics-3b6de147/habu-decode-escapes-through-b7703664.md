---
title: Decode escapes through one table
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T07:46:08.771975+01:00"
closed-at: "2026-11-16T04:07:36.493490+01:00"
---

Problem: src/core/checker.f keeps a second escape table and decoder (ESC-BYTE? :19916 .. ESC-DECODE :20014) beside src/habu/escape.f's. Acceptance: one table, in ESCAPE; the checker, verify-source.f and source-discovery.f decode through it; output unchanged. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).

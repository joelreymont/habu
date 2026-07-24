---
title: "Tools: migrate JSON file records"
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-24T13:12:30.395583+02:00\\\"\""
closed-at: "2026-07-24T17:43:03.148083+02:00"
close-reason: Reviewed implementation landed and verified at master@origin 4fb6f52fb815.
---

Why: tools/json-file.f still declares JSONF:line and JSONF:row with legacy PRODUCT, blocking total declaration-event release. Owner: those two declarations in tools/json-file.f and tools/json-file-test.f only. Replace both PRODUCT blocks with STRUCTURE inside package JSONF, preserving field names/order and ptr u8/n schemas, JSONF-LINE:MAKE/UNMAKE and JSONF-ROW:MAKE/UNMAKE spelling, two-cell and three-cell layouts, OPTION presence semantics, trailing-EOF line behavior, row kind/code meanings, dynamic line growth, errors, and allocation ownership. Update stale comments. Forbidden: aliases, legacy parser edits, JSON parser redesign, option changes, raw casts, buffer/lifetime changes, unrelated cleanup. Acceptance: the real json-file cursor suite proves data, blank, error, EOF, trailing partial line, growth, MAKE, UNMAKE, and line counters before/after; exact reflection/layout and public effects remain stable; no executable PRODUCT remains in tools/json-file.f; focused typed-local/package/trust/filemap gates pass. Dependency proof: master 227b5b349702 has green unified STRUCTURE and json-file baselines.

Claim: agent=codex-json-records workspace=.jj-ws/habu-tools-migrate-json-6a3f0bea

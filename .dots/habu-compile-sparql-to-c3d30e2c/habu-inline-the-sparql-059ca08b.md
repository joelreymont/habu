---
title: "Inline the SPARQL runtime's SQL functions"
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T19:14:40.376590+01:00"
---

Problem: a LANGUAGE sql function inlines only when an IMMUTABLE body calls only immutable functions and a STRICT body is strict; src/lib/sparql/runtime.sql's and test/sparql/host.sql's term functions call jsonb_build_object (STABLE) or <text> || <non-text> (anytextcat, STABLE), so a compiled statement calls them once a row (measured on PostgreSQL 18: 1,036 ms per million against 232 ms inlined). Acceptance: every such function inlines (EXPLAIN VERBOSE shows its body), answers unchanged for every input, NULL included; runtime.sql's header states the rule. Depends: none. Claim: unassigned.

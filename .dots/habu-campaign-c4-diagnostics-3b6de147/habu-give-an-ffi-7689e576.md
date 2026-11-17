---
title: Give an FFI declaration one record in every reader
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T23:03:37.968706+01:00"
---

Problem: /tmp/jerry-inputcol/ffiinc.f ('FUNCTION: foo ( -- )' then an include, no library selected) is E-FFI-LIBRARY at 1:1 under hb --load and E-FFI-SYNTAX 'expected ( after the C symbol' at 1:17 under tools/check.f: the load checks the library first and the verifier the syntax first. Acceptance: the load, tools/check.f and --verify-only print one and the same record for it; a test fails first. Depends: none. Claim: unassigned.

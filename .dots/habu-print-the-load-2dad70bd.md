---
title: "Print the load's FFI syntax records in check.f"
status: active
priority: 2
issue-type: task
created-at: "2026-11-14T23:10:31.933931+01:00"
---

Problem: src/habu/verify-source.f FFI-STOP (16 sites) stops tools/check.f with a bare E-FFI-SYNTAX E-STATEMENT-THROW for a FUNCTION: clause the load rejects with a placed record, message and help (src/lib/ffi-abi.f SYNTAX-REJECT and its callers). Acceptance: check.f prints the load's record for each clause rejection, one text for both, as check.f tests. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-reader.

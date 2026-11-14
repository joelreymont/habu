---
title: "Print check.f's checker records as prose by default"
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T23:10:31.944369+01:00"
---

Problem: tools/check.f prints checker rejection records as JSON without --json-errors (a body rejection, the FFI arity record), while its other diagnostics are prose unless --json-errors is given (tools/check-core.f CHK-JSON). Diagnostics are Rust-style prose for the agent that reads them. Acceptance: without --json-errors check.f prints each checker record in prose, with --json-errors as JSON, as a check.f test. Depends: none. Claim: unassigned.

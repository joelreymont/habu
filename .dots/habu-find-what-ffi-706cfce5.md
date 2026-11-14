---
title: Find what FFI role, bool and enum inputs need
status: active
priority: 2
issue-type: task
created-at: "2026-11-14T23:57:06.716000+01:00"
---

Problem: a FUNCTION: input typed as a role (fd, idx), bool or an ENUM fails with 'non-certified definition ... at VALUE!' instead of an FFI record: IN-TOKEN (src/lib/ffi-abi.f) takes any type EFFECT:READ-TYPE reads as K-VALUE, and STAGE-VALUE's VALUE! takes n. Acceptance: a hunt reports which input and output types fail and where, what docs/forth.md says FUNCTION: takes, and what Zen, Tender and Maki declare; the fix is designed from it. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-ffitype.

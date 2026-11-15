---
title: Place a derive payload rejection at its payload
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T12:01:08.701429+01:00"
---

Problem: src/core/sumtype.f TDECL-DERIVE-NODE-OK and TDECL-DERIVE-FIELD-OK run once the declaration has parsed and name the registry's copy of a name (`ff TFAM-NAME$`, `CT-NAME$`) or the declaration's name (TDN-A), never the payload's own token, so a rejection such as 'field family must also derive eq' or 'pointer payloads have no derived equality' is not placed at the payload. Acceptance: each names the payload's type token at its place; a diag-position test. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-dpos.

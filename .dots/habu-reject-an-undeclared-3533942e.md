---
title: Reject an undeclared immediate under --verify-only
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T22:45:31.454857+01:00"
---

Problem: a body calling an immediate with no parse-imm declaration (/tmp/carl-pimm/fx/none.f, a-own.f) is E-UNMODELED-IMMEDIATE under --load and the default tools/check.f, but 'tools/check.f --verify-only' accepts it silently (rc 0): the verifier alone does not reject an unmodelled immediate. Acceptance: --verify-only rejects it as the load does. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-vfix.

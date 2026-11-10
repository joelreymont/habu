---
title: A packet for a definition with no signature omits declared_effect
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T11:11:05.664796+02:00"
---

Problem: every refusal packet from a definition with no signature leaves out `declared_effect`, which tools/gate-json-assert-core.f:468-469 requires to be null or a string, so `tools/gate-json-assert.f diag-contract` fails with `missing JSON field` on it, a plain E-MISMATCH included (/tmp/bob-review-unck/ gja-nosig-mismatch.json). Both engines. Found by the review of habu-tokens-refused-as-e0ec22ab. Source: Joel, 2026-11-10: "you must dot and launch a fix automatically for any issues you identify". Acceptance: the packet carries `declared_effect` as null when the definition declares none (the contract in tools/diag-code.f and the gate agree on which), and diag-contract passes on a no-signature refusal. Files: src/core/render.f (DIAG-JSON, DECL-EFFECT), tools/gate-json-assert-core.f only if the contract is the wrong side. Verify: the gate on a no-signature mismatch and on a recurse-nosig packet; a row where the packet fields are tested. Depends: habu-render-the-declared-f4f40bb8 landed (same words). Ownership: the checker's diagnostics. Claim: unassigned.

Claim: agent=bob workspace=.jj-ws.noindex/habu-a-packet-for-72692145

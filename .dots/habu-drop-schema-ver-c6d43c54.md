---
title: Drop schema_version from the public-signatures manifest
status: active
priority: 2
issue-type: task
created-at: "2026-11-14T21:44:37.061347+01:00"
---

Problem: tools/public-signatures-core.f:571, :778, :813, :867 write schema_version 1 into every manifest record and the document; tools/gate-json-assert-core.f GJA-PUBLIC-SIGNATURES checks it. Habu has no format versions (hard cutover). Acceptance: the manifest carries no schema_version and its check does not ask for one. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-psig.

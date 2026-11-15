---
title: "Check a generated name against the source's seals"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T08:22:36.688083+01:00"
closed-at: "2026-11-16T04:07:36.642527+01:00"
---

Problem: --verify-only accepts a generated name in a package the source sealed itself (/tmp/jerry-genfix/fx/seal.f) that the load and tools/check.f reject; xref.f GENERATED-DECL-NAME-PREFLIGHT asks the engine's dictionary, the verifier keeps the source's seals in verify-source.f SEALS. Acceptance: the three readers give one placed record, rc 70. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).

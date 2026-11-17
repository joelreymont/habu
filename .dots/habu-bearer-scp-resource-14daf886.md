---
title: BEARER scp, resource metadata and https-only URLs
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T15:34:08.811651+01:00"
---

Goal: mark's requests (/tmp/mark-bearer-requests.md): CLAIM-HOLDS? answers a space-separated string claim (scp) by its words; one word answers the RFC 9728 protected-resource metadata GET from what METADATA! and ISSUER! hold (Zen chat.f, Maki service.f:706, sapd serve.f build it by hand); ISSUER!, CLIENT! and METADATA! reject a URL that is not https unless its host is a loopback IP literal. Acceptance: bearer-test covers each; Zen, Maki and sapd can drop their metadata bodies. Depends: Joel's names. Claim: unassigned.

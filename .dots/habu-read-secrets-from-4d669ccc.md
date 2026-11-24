---
title: Read secrets from Azure Key Vault in SECRET
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T19:30:09.714142+01:00"
---

Goal: joel-bb (/tmp/joel-bb-prod1-requests.md, Habu section): Tender's read-only Key Vault store (tender tenderd/secrets/azure.f) moves into Habu beside SECRET:DIR-BIND (src/lib/secret/dir.f), with the IMDS token fetch as a public word; Zen (credentials.f:116-125), sapd and tenderd bind it. Acceptance: a program binds the store by vault name and reads a secret through SECRET's interface; a hermetic test reads through a loopback IMDS and vault; the token word answers a token for a resource. Depends: Joel names the public words (design /tmp/jerry-p1/design-azure.md). Claim: agent=jerry workspace=none (design).

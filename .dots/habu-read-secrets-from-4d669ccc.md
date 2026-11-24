---
title: Read secrets from Azure Key Vault in SECRET
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T19:30:09.714142+01:00"
blocks:
  - habu-get-an-azure-1131dbd9
---

Goal: joel-bb (/tmp/joel-bb-prod1-requests.md, Habu section): Tender's read-only Key Vault store (tender tenderd/secrets/azure.f) moves into Habu beside SECRET:DIR-BIND (src/lib/secret/dir.f), with the IMDS token fetch as a public word; Zen (credentials.f:116-125), sapd and tenderd bind it. Acceptance: a program binds the store by vault URL (SECRET:AZURE-BIND) and reads a secret through SECRET's interface; a hermetic test reads through a loopback IMDS and vault; the token word answers a token for a resource; the vault URL check moves from BEARER's private SECURE into URI's private section, shared by both. Depends: habu-get-an-azure-1131dbd9 (design /tmp/jerry-p1/design-azure.md). Claim: unassigned.

---
title: Read secrets from Azure Key Vault in SECRET
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T19:30:09.714142+01:00"
---

Goal: joel-bb (/tmp/joel-bb-prod1-requests.md, Habu section): Tender's read-only Key Vault store (tender tenderd/secrets/azure.f) moves into Habu beside SECRET:DIR-BIND (src/lib/secret/dir.f), reading its token through BEARER:IDENTITY-TOKEN; Zen (credentials.f:116-125), sapd and tenderd bind it. Acceptance: a program binds the store by vault URL (SECRET:AZURE-BIND) and reads a secret through SECRET's interface; a hermetic test reads through a loopback IMDS and vault; the vault URL check moves from BEARER's private SECURE into URI's private section, shared by both. Design: /tmp/jerry-p1/design-azure.md. Depends: none. Claim: unassigned.

---
title: Get access tokens from an issuer, and a loopback issuer
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T22:40:44.025516+02:00"
---

Lane Zen-H4 (after MCP-L1): BEARER CLIENT!, grant, TOKEN, E-TRANSFER, E-REFUSED (zen-h-design.md 2.C), plus phil's loopback OIDC issuer (discovery, JWKS, token endpoint for client credentials and OBO, signed with JWS-FIXTURE; a library beside BEARER whose routes a test registers on its own server, and a child-process tool printing its port) shared by Habu, Zen and Tender tests. The issuer's public words and command line go to Joel before the lane starts; then tell phil.

Lane H4a (TEST-ISSUER, approved by Joel): brief-zen-h4a.md Revisions 1-2, done as skyotvxl 0cedc21c on twotzuqz in .jj-ws.noindex/carl-iss; Opus review running. H4b (TOKEN) waits for MCP-L1 and H4a. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-iss.

Status 2026-10-30 late: H4a fold-in lkzzuuuq 8f39ffae ACCEPTED (carl-iss). H4b (CLIENT!, grant, TOKEN, E-TRANSFER -9630, E-REFUSED -9631): brief brief-zen-h4b.md, worker running in .jj-ws.noindex/carl-h4b on the merge of qtwvmrum and lkzzuuuq, tested against TEST-ISSUER in test-issuer-test.f. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-h4b.

Status 2026-10-31: H4b Opus review REVISE (E-FIRST/E-LAST pair, FETCH handle leak, TOKEN before CLIENT!, FORM grow wipe, EXCHANGE comment, two tests). H4b duplicated onto carl-int as wouzlrrn cd5a6ff0 on ltoosomn; fold-in worker running (brief-h4b-fold.md). Claim: agent=carl worker workspace=.jj-ws.noindex/carl-int.

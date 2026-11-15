---
title: Share one form decoder in HTTP
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T12:42:03.896522+01:00"
---

Problem: src/lib/net/test-issuer.f decodes application/x-www-form-urlencoded bodies with its own DECODE+/DECODED/PAIR, and src/lib/net/http-request.f PARAM+ decodes queries with private FORM-BYTE/FORM=/FORM-APPEND/PAIR: two decoders of one encoding. Acceptance: one decoder in HTTP that both use, test-issuer's copy deleted, http-test.f passing. Depends: none. Claim: unassigned.

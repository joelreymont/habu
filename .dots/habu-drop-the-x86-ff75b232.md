---
title: Drop the x86 quotation-local workarounds
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T14:26:07.186317+01:00"
---

Problem: the false claim that 'x86-64 refuses a quotation that reads a local' remains at src/lib/net/http-response.f:272 (WRITE-JSON, run inside JSON! under finally with JSON-BODY as its carrier), test/checker-verify-pkg-scope.f:21, test/checker-dup-record.f:23 and test/certify-does-definer.f:344. HTTP:JSON! stores its body quotation, so http-test.f's ITEM-* cells must stay until it does not. Acceptance: no comment makes the claim; JSON! keeps no body past its run, or the cells' reason is the escape rule; the http suite passes on spark and on pulsar's native engine. Depends: none. Claim: unassigned.

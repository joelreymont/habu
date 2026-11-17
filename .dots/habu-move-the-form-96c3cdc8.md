---
title: Move the form decoder into URI
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T08:00:01.899182+01:00"
---

Problem: HTTP:FORM-DECODE+ (src/lib/net/http-request.f:996, with its private FORM-BYTE :961 and FORM= :975) decodes a web form inside HTTP while URI holds the encoder; src/lib/net/test-issuer.f:147 and :152 call it. Acceptance: URI:FORM-DECODE+ decodes a web form; HTTP's copy is deleted and its callers call URI's; uri-test.f decodes what FORM-ENCODE wrote back to the bytes. Depends: none. Claim: unassigned.

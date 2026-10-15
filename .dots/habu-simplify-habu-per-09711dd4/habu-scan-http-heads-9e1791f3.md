---
title: Scan HTTP heads and headers once
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:30.980326+03:00"
---

Problem: src/lib/net/http-request.f:181-189 HEAD-END rescans the head from byte 0 after every fill (READ-HEAD :759-775); with a peer sending one byte at a time that is about 25M probes under HEAD-CAP $1C00. READ-BODY (:742) binds tea and teu and never uses them. CHUNKED? (:734) and CONTENT-LENGTH (:720-731) scan the headers again, about five linear scans per request. src/lib/net/ws.f:776-781 LISTED? duplicates http-request.f:563-569 LIST-HAS?, and ws.f:785 SOLE scans twice. Acceptance: the head-end search resumes at max(0, previous end - 3); each request resolves its headers in one scan; ws.f uses the HTTP token-list matcher; existing HTTP and ws tests green, plus a byte-at-a-time peer case covering head assembly. Fable review. Files: src/lib/net/http-request.f, src/lib/net/ws.f, their tests. Verify: http-request and ws test rows in test/gate-stdlib-cases.f. Depends: none. Ownership: listed files. Claim: unassigned.

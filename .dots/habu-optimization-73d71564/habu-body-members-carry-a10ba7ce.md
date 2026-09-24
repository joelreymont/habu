---
title: Body members carry every later function of their record
status: open
priority: 3
issue-type: task
created-at: "2026-09-22T10:02:32.025224+03:00"
---

Problem (measured by the quot-adr lane): under tier 1 (tools/hb-build.f) a quotation reference is an ADR, so src/habu/aot-closure.f BODY-END-SCAN sees no movz/movk chain marking a function boundary and an anonymous body member runs to its record's end. On `defer A  : INSTALL ( -- ) [: INC ;] is A  [: DEC ;] is B ;` the first body is a 32-byte member in a 116-byte record: its own 16-byte function plus the second quotation's 16 bytes, which the second body carries again as its own member (rec-in-clo=0: the record itself is not a member, so the duplication is between sibling bodies, not through the record). Acceptance: measure the duplicated bytes on a real application image (tools/image-size-lib.f or the aot call report on Tender's build), then either bound a body by the next function's start regardless of tier (the front end numbers a quotation after the function its `[:` stands in, so the boundary is known without decoding bytes) or state why the duplication is below the noise. Files: src/habu/aot-closure.f (BODY-END-SCAN), src/habu/aot-lib.f. Verify: test/stripped-quotation.f, test/gate-aot-positive.f, tools/hb-build-test.f. Depends: none. Ownership: hazel. Claim: unassigned.

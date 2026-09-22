---
title: "RECORDED-LEN drops the RET of a record's last empty quotation"
status: open
priority: 3
issue-type: task
created-at: "2026-09-22T10:02:32.027529+03:00"
---

Problem (found by the quot-adr lane reading src/compiler/native/publish.f, not yet reproduced): RECORDED-LEN drops a trailing RET when it records a definition's length, so an empty `[: ;]` that is the LAST function of a record starts exactly at the member's end. src/habu/aot-lib.f MAP-IN-MEMBER treats a target at a member's end as the next member's start, so ADR-TARGET! refuses the quotation's ADR as outside its member (the old MAP-TARGET! path refused it through OLD>NEW as well). Acceptance: a fixture whose record ends in an empty quotation (`: W ( -- ) [: ;] is X ;` as the last function of the record), built stripped in the tools/hb-build-test.f shape, measures the refusal first; then either RECORDED-LEN keeps the RET of an empty function or the closure walk carries the byte, and the fixture pins it. Files: src/compiler/native/publish.f, src/habu/aot-lib.f, src/habu/aot-closure.f. Verify: test/stripped-quotation.f, tools/hb-build-test.f. Depends: none. Ownership: hazel. Claim: unassigned.

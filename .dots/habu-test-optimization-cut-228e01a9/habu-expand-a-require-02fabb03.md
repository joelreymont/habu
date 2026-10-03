---
title: Expand a require where it sits in check.f
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T04:12:40.682337+02:00"
---

Problem: tools/check-core.f:553-569 CHK-EXPAND-ID pushes every file a source requires ahead of the source itself, wherever the require sits, so a word the source defines before its require is undefined in the required file when checked, though the real load path defines it first. Reduced: a.f `: RC-SECRET ( -- n ) 5 ; require rc/b5.f` with rc/b5.f `: RC-USE ( -- n ) RC-SECRET ;` loads rc 0; tools/check.f a.f is rc 70. In the tree it hides REC-STATE@ ordering in lib aio, curl, tcp4, udp4, pg, serial, serial-xmodem and signal, and the MY-SLOT path in lib/net/http-request.f after plan 15. Acceptance: check.f orders definitions as the load path does (a require is expanded where it sits); the reduced case checks rc 0 and a real use-before-definition is still refused E-UNDEFINED; the listed libraries check as they load; cases through tools/check-test-lib.f written before the code. Files: tools/check-core.f, tools/check-test-lib.f. Verify: tools/check-test.f. Depends: habu-let-check-f-f02aa703 (r4-render, plan 15). Ownership: CHK-EXPAND-ID order.

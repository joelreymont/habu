---
title: Find why http-test F168 and F171 fail under load
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T08:34:57.355825+03:00"
---

Problem: src/lib/net/http-test.f cases F168 and F171 fail 'assert: expected true got false' in a full native suite run (~/.cache/tmp/jerry-int6/full.log:1221-1229, load 9-60; requests r0-4 throw -3900, r0-11 and r0-13 throw -9990 just after), and F168 failed the same way in ~/.cache/tmp/habu-origin-gate-93456/native-gate.log:1548. The suite passed standalone three times (~/.cache/tmp/jerry-int6/http-{1,2,3}.log) and in the full rerun. Suspected but unproven: a deadline under load (COLLECT-MS 2000 ms, http-test.f:703). A deadline read as a false assertion is against docs/gate.md:426-440. Acceptance: reproduce under a bounded load driver with a private HB_TMP and name the cause; fix the responsible layer. If it is the test's deadline, an expiry reaches the pool as E-PROC-TIMEOUT rather than a false assertion and the wait is sized from a measurement under load; if it is the server (src/lib/net/http.f), fix it there with a case that fails before the fix. Ten runs under load green, assertions unchanged. Files: src/lib/net/http-test.f; src/lib/net/http.f only if the cause is there. Verify: the http SUITE row ten times under load. Depends: none. Coordinate: carl edits HTTP request/router body handling (his http-route-cap lane); tell him before any src/lib/net/http.f change. Related: habu-size-the-http-c94ce7d8, habu-keep-the-curl-9082fbfa. Ownership: src/lib/net/http-test.f. Claim: unassigned.

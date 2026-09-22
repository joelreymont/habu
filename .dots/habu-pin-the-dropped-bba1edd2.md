---
title: Pin the dropped forged-tag fetch in native code
status: open
priority: 3
issue-type: task
created-at: "2026-09-22T11:02:26.099274+03:00"
---

Closed dot 38ad40e8 named two forged-tag cases. The used case is pinned: test/compiler/native-fetch-check.f (the descriptor check directly), test/runtime-regression-test.f BAD-WIDE and BAD-NARROW (the value is printed) and test/gate-aot-positive-lib.f PRESEED-FETCH (HLP fetches and MATCHes in a stripped image). The dropped case is not: cedar's subject (2026-09-14, then /tmp/cedar-typed-fetch-native-subject.f) stored tag 5 into an allocated LAYOUT-BUFFER, fetched it, dropped it and printed a marker; the native image exited 0 and printed the marker while the JIT exited 85 with `hb: bad layout tag`. No suite fetches a forged tag and drops it through the native or the stripped-image path, so a lowering that elides an unused load would take the check with it unseen. Acceptance: a fixture in the native path and one in the stripped-image path that forge a tag, fetch, drop and print, each expecting exit 85, `hb: bad layout tag` and no marker.

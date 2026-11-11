---
title: Admit a caller-bound callback slot in the x86-64 thunk
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T18:23:41.237253+02:00"
---

Problem: TASK:CALLER-CONTEXT (dot habu-route-libpq-notices-a88cc555, workspace pg-notice) is implemented in the arm64 thunk only (src/habu/habu1.f BCALLBACK-THUNK). The x86-64 thunk (src/habu/kernel-x64.f :4837-4917) reads a caller-bound row's owner (4*n + CB-ROW-CALLER) as another thread's and exits 106, so src/lib/pg.f's notice processor ends an x86-64 process at the first notice. Acceptance: the x86-64 thunk admits a caller-bound row as the arm64 one does (count the thread in, find its region through CB-TLS-GET and CB-TLS-KEY, count it out), and the three caller-context cases of src/lib/ffi-callback-test.f run on x86-64 with their [if] removed. Files: src/habu/kernel-x64.f, src/lib/ffi-callback-test.f, test/ffi-callback-fixture.f, test/ffi-callback-child.f, test/x86-64-kernel-callback.f. Verify: the ffi-callback suite on pulsar. Depends: habu-route-libpq-notices-a88cc555 landed; x86-64 work is set aside until the three consumers are released. Consumers: none meets it today (none runs on x86-64). Ownership: the x86-64 thunk. Claim: unassigned.

From the pg-notice review: on x86-64 a caller row reads as 'slot held by another thread' (src/habu/kernel-x64.f :4850-4852), so pg.f there exits 106 on the first notice where master printed a bare line, and pg-test's NOTICE-CASES would end the test process; NOTICE-CASES is switched off on x86-64 with the caller cases and comes back on with them.

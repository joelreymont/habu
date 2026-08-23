---
title: schedule-lint counts never-run registrations as covered
status: active
priority: 1
issue-type: task
created-at: "2026-08-22T22:47:07.014416+02:00"
---

Problem: tools/lint/schedule-lint.f:520-533 SLICE-AT marks a slice live for every non-retired phase whose STDLIB-SLICE? is true without a STARTED? test (the comment at :504-509 admits it); only phases 4 (tail) and 40 (proof) spawn test/gate-stdlib.f with a slice (test/run-lib.f:1189-1199 PHASE-RESIDENT?; 18/19 deferred at :138-139); every other stdlib phase forks test/run-worker-stdlib.f:20-50 which runs GSI inline bodies and never evaluates a SUITE registration. 27 registered files are therefore 'covered' and never executed: tools/lint/shadow-lint-test.f (test/gate-stdlib-lint-tools.f:48 includes only the tool), tools/imgdump-test.f, tools/imagedisasm-test.f, five under ptx-stdlib (lib/ptx/sentinel-test.f, cuda-driver-test.f, cuda-scope-test.f, ad-gen-test.f, src/arch/ptx/vjp-test.f) and 19 under ptx-toolchain (test/gate-stdlib-inline-lib.f:889-900 admits it). test/run-lib.f:129-137 claims the deferred aggregates' members run elsewhere; GSI-LINT-ARTIFACTS-FAST (:903-913) does not list imgdump/imagedisasm. Deleting every assertion in shadow-lint-test.f does not flip the gate. Acceptance: LABEL-COVER? counts a slice only when a STARTED, non-resident phase asks for it; resident coverage comes from GSI lists alone; the 27 files scheduled or their registrations retired; a hostile fixture (a registration with no runner) reds the lint. Files: tools/lint/schedule-lint.f, test/gate-stdlib-cases.f, test/gate-stdlib-inline-lib.f. Verify: schedule-lint 0 unreached with the fixture red. Depends: none. Ownership: gate runner. Claim: agent=sched-cover workspace=.jj-ws/habu-schedule-lint-counts-9eaac4d2

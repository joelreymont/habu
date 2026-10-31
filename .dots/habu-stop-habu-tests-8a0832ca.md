---
title: Stop Habu tests leaking temp dirs into TMPDIR
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T20:11:15.463080+02:00"
---

Problem: on 2026-10-31 Joel's /tmp held 116 GiB in 5,750 entries, 5,591 created Oct 27-31, and many were Habu test leftovers. Named families: native-build-edit-e2e-*-0 (test/compiler/native-build-edit-e2e.f, up to 1.6 GiB each, five in one agent's TMPDIR), native-file-types-* (11-15 copies of each fixture), native-file-retired-* (test/native-file-retired-e2e.f), hb-lsp-entry-* (58), habu-zip-source-* (64), hb-generations-* (41), habu-lsp-test-* (39), habu-aio-* (31), hb-build-native-* (25), hb-native-smoke-* (14), prefix-provided-unfilled-* (33), check-drivers-* (13) and .tmp-<n>-0 files. Agents also trashed d/d/d fixture trees 450-2,713 levels deep, which Finder cannot delete (their scratch held fs-mutate-test.f). It is not known which of these leak from passing runs and which from failed or killed runs. Acceptance: (1) a census naming each test that leaves anything in its TMPDIR after a passing run, a failing run and a killed run; (2) the owning layer fixed so that a complete test run, pass or fail, leaves its TMPDIR as it found it, and a killed run's leftovers are removed by the next run instead of accumulating; (3) a check in the suite that fails when a run leaves anything behind. Files: test harness and the tests the census names. Verify: run the gate with a fresh empty TMPDIR and list it afterwards; repeat with one failing and one killed test. Depends: none. Ownership: as the census names. Claim: unassigned.

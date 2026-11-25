---
title: "Test the loader's remaining rejections and scopes"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T16:53:28.699666+02:00"
---

Problem: the feature review of habu-keep-active-src-2da0b7b5 found behaviour with no test: (1) a rejection in an 'hb X.f' program names ' at X.f:<line>' and a missing program exits 74 with 'include: cannot open <canonical path>' (src/core/include.f RUN-PROGRAM); (2) SOURCE-ROOT:ROOT! inside a SOURCE-ROOT:WITH body, whose end restores the caller's root; (3) SOURCE-ROOT:WITH-BYTES and WITH-SUPPLIED called by a word during a build are rejected by name (WITH-SUPPLIED probed 2026-11-10 at 'Reject provided from a word in a build': exit 74 with the build rejection and the path); (4) a file whose top level says provided builds, and builds again from its object, where LINK-ACTS replays the provide through SOURCE-ROOT:BUILD-REJECT (probed the same day: two builds into one HABU_OUT, each exit 0; whether the second linked the object was not looked at); (5) a word's provided of a file the registry already holds is rejected in a build, where it returned with nothing stored before, and a word's required of such a file is not rejected (review of 'Reject provided from a word in a build'; read from the code, not run). No behaviour defect is known. Acceptance: each of the five has a case that fails when the behaviour is removed. Files: test/aot-shifted-bytes.f, test/file-objects.f, test/source-root-test.f, test/source-root-exe-test.f. Verify: those suites. Depends: none. Ownership: none. Claim: unassigned.

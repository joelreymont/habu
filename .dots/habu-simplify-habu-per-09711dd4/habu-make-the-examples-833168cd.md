---
title: Make the examples self-loading programs
status: closed
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:31.021086+03:00"
closed-at: "2026-10-15T18:28:00.339119+03:00"
close-reason: "Deleted the five example assertion suites and tools/examples-test.f: every assertion is a library test."
---

Problem: src/examples/{string,array,file-map,build-script,property-test}.f are T= assertion suites that duplicate library tests. They have no require lines and depend on tools/examples-test.f bundling (EXT-BUNDLE-* :212-245), registered at test/gate-stdlib-cases.f:402. Acceptance: each example either requires what it uses and runs as a program, or is deleted where it only duplicates a library test; the bundling machinery in tools/examples-test.f is deleted; the registry row updated. Files: src/examples/*.f, tools/examples-test.f, test/gate-stdlib-cases.f. Verify: the examples row. Depends: none. Ownership: listed files. Claim: unassigned.

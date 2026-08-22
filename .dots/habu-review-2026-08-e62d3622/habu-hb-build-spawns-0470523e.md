---
title: hb-build spawns a child engine per lint
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:47:07.168129+02:00"
---

Problem: tools/hb-build-lib.f:341-357,427-442 HBB-INSTALL-CHILD-LINTS (the default) runs each lint in a child hb with a 10-file --load and a 65 KiB capture cap; tools/hb-build-direct-lints.f:1-44 installs the hooks in-process but is loaded by three test files only; tools/hb-build.f never loads it. Acceptance: the direct install is the default; the child command builders deleted; hb-build tests green. Files: tools/hb-build-lib.f, hb-build-direct-lints.f, hb-build.f. Verify: hb-build tests; one build shows no child spawn. Depends: none. Ownership: build tool. Claim: unassigned.

---
title: A build cannot name the object of a source whose path nears PATH-CAP
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T16:28:40.198576+02:00"
---

Problem: an object's path is the out root plus obj/<stamp>/aarch64-apple-darwin/app plus the source's absolute path, so a source whose absolute path plus that prefix exceeds PATH-CAP (1023) throws -2106 E-FS-CAPACITY in a build where hb --load of the same source succeeds (root-facts B2b worker: a helper under a 1019-byte root). A build fails where a load would not. Acceptance: a build names every source hb --load accepts. Files: src/habu/linker.f (object naming), test/aot-shifted-bytes.f. Verify: the long-root case builds with its helper under the root. Depends: habu-reuse-the-obj-a450cc5b. Ownership: those files. Claim: unassigned.

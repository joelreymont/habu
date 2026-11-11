---
title: Refuse at the check a primitive the built tree no longer has
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T11:53:29.046895+02:00"
---

Problem: in a build, a primitive the tree being built has removed from src/habu/prims.f is still typed by the running hb own record of it (READ-BOUND skips hb records only for symbols the kernel object stated), so the checker accepts a call to it and the link refuses later, rc 74, link: MAIN names fence, neither a product word nor a kernel primitive. Found by the feature review of privacy 2c. Reproducer: tree ~/.cache/tmp/priv-2c/review/e5 (fence row and body removed), app ~/.cache/tmp/priv-2c/review/p/app-fence.f built by ~/.cache/tmp/priv-2c/p2/g2. Acceptance: the check refuses the call as undefined at its token, with file and line; the link line is not reached. Files: src/core/checker.f (READ-BOUND, PREFIX-SYM?). Verify: the reproducer exits with the undefined-word refusal; engine fixpoint unchanged. Depends: habu-a-build-s-87c9c7c2 landed. Ownership: unassigned. Claim: unassigned.

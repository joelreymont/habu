---
title: Remove superseded kernel objects from the out root
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T07:43:32.003674+02:00"
---

Problem: each kernel build whose inputs changed writes a new content-named kernel-engine-<hex>.hko or kernel-app-<hex>.hko (tools/hb-build-core.f KERNEL-PATHS, kernel reuse) of about 280 KB and nothing removes the old ones, so an agent iterating on kernel files grows the out root without bound; the hashed names before kernel reuse grew the same way. Acceptance: an out root keeps a bounded number of kernel objects per tree and kind, and no build or child process ever reads a removed one (a concurrent build's object is never removed while that build can still read it). Depends: habu-reuse-the-kernel-7955ab15. Claim: unassigned.

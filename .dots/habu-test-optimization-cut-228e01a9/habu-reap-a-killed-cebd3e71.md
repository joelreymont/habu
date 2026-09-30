---
title: "Reap a killed keyed-image builder's work directory"
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-30T18:18:08.096437+02:00\""
---

Problem: a keyed-image builder (test/keyed-image.f) creates its work directory inside the shared build cache (~/.cache/habu-build) so the finished image can be published by an atomic rename, and only the builder's own exit removes it. A builder killed by the pool's deadline, by the signalled gate root (change monlstxr kills the whole tree) or by SIGKILL leaves the directory behind; on 2026-09-30 eight such directories sat in ~/.cache/habu-build, most from other agents' runs. Acceptance: a killed builder's work directory does not outlive the next builder of any key in that cache (reaped by the next build that proves its owner dead, or placed where the killer's cleanup already removes it while keeping the atomic publish); concurrent live builders are never disturbed; an E2E case kills a builder mid-build and asserts the directory is gone after the next build, and a live concurrent builder's directory survives. Files: test/keyed-image.f and the cache layer it uses (lib/build-cache.f), docs/gate.md. Verify: the new case, the keyed-image rows. Depends: none. Ownership: keyed-image work directories. Claim: agent=kestrel workspace=.jj-ws/r4-reap.

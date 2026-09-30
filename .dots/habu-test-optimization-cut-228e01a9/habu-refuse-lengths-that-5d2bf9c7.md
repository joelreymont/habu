---
title: Refuse lengths that wrap a capacity guard
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-30T16:51:16.792686+02:00\""
---

Problem: guards of the form `off + u + 1 > cap` wrap for a huge u and pass a negative one; `>LEN`, `>OFF` and friends are unchecked casts. 3c49e2f1 fixed PROC-ZCOPY and PROC-ARGV-FITS?; the class was not audited. Known: src/os/env-base.f TMP-PATH (`TPQ @ 1 + TPU @ + TMP-PATH-CHECK`) is owned by the path-capacity dot. Acceptance: every guard in lib/ that adds a caller-supplied length or offset before comparing is listed; each reachable one refuses negative and wrapping values with its existing error, by comparing against the room left; boundary tests (exact fill, one past, -1, maximum cell) are written first through the public word. Files: lib/*.f and their tests. Verify: the owning suites. Depends: 3c49e2f1. Ownership: guard expressions in lib/; not capacity constants, not src/. Claim: agent=kestrel workspace=.jj-ws/r4-wrap.

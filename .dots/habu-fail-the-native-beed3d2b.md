---
title: Fail the native build when its support image fails
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T22:15:36.588899+02:00"
---

Seen once by lane dtail's fourth fold-in worker (workspace .jj-ws.noindex/jerry-dtail, tree 99656612, load 5-27, 2026-10-31): `$HOST --load tools/native-build.f -- .out/cand/hb` (fnd-head.md's candidate build, HOST = the public hb) printed "native-build-support: image build failed" (tools/keyed-image.f:190 BUILD-RUN, `rc die`) and still exited 0; the log was overwritten by the next build, which passed. A failed step behind a zero exit is a swallowed error: either the caller catches the die and carries on without the support image, or the child's rc is lost. Root-cause first: reproduce by making the support image's build fail (for example a refused definition in a file the support program requires) and read who catches it. Acceptance: that run exits non-zero and names the failure; no build output is published from it.

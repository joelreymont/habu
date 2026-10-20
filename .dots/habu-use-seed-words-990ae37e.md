---
title: Use seed words in primitive state
status: active
priority: 0
issue-type: task
created-at: "2026-10-15T08:41:10.069633+03:00"
---

Fresh combined-source recovery e028a6df fails its first bootstrap stack image
with exit 70 at `0<>`; correcting those uses exposes the same problem at `0>=`.
The new checker `PRIMITIVE-STATE` loads before the prelude defines these words.
Use the existing seed operations: `0= 0=` for nonzero and `0< 0=` for
nonnegative. Audit the whole new routine against the seed vocabulary; do not
add primitives or unchecked escapes. The existing `bootstrap-engine-stack.fs`
provides the failing E2E. Acceptance is a green stack test and private check-only
recovery on the corrected combined source, followed by matching native
qualification. The source corrections are 1faa1aca and c1c992d0; focused
independent review is clear, while recovery remains in progress.

---
title: Load hb-build without recompiling it
status: open
priority: 3
issue-type: task
created-at: "2026-10-22T14:59:32.133121+03:00"
---

Problem: every `bin/hb --load tools/hb-build.f` compiles hb-build's own files (tools/hb-build.f, tools/hb-build-lib.f, tools/hb-build-core.f) from source before it builds anything: hb does not carry them (tools/hb-build-core.f RUNTIME-FILES lists what it carries). Joel (2026-10-31) rejected a separate hash-of-sources check: "why are we developing incremental building if we are gonna check hashes of source files?". Acceptance: hb carries hb-build's files, so an invocation compiles none of them; no cached image and no hash of sources; a no-change build's wall time measured before and after; outputs unchanged. Depends: none. Claim: unassigned, owner jerry.

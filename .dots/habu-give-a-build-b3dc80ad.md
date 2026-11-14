---
title: Give a build its out directory without HABU_OUT
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T22:34:19.492479+02:00"
---

Problem: a build finds where to write through the environment variable HABU_OUT (src/lib/build-out.f:1, read at :29 through ENV-INPUT:CURRENT$); tests set it for children (test/linear-suite.f:376, test/file-objects.f:243 and :568, test/aot-shifted-bytes.f:176 and :195, tools/lsp-entry-test.f:66). An environment census classed it replaceable by an argument of the build word in the one-process build. Source, Joel 2026-10-29: remove env vars except where Habu really needs one and cannot do without it. Its replacement is a change to the build words' public interface nobody has settled. Acceptance: a design for how a build is given its out directory, approved by Joel and recorded here; then HABU_OUT is read and set nowhere. Files: src/lib/build-out.f, its callers, and the tests named above. Verify: rg -n HABU_OUT prints nothing outside .dots. Depends: none. Ownership: unassigned. Claim: unassigned.

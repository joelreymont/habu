---
title: "A kernel built from outside the tree is filed by the caller's directory"
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T12:16:09.158450+01:00"
---

Problem: tools/hb-build-core.f KERNEL-PATHS (:388) names the kernel object directory after SOURCE-ROOT:CWD$ and KERNEL-BUILD (:523) runs under SOURCE-ROOT:CWD$ ... SOURCE-ROOT:WITH, while their comments say the tree root; a build run from outside the tree files and reads its kernel object under the caller's directory, so builds from two directories never share one (found by the --repl path lane). Acceptance: both use the tree the build reads hb from (SOURCE-ROOT:ENGINE$, as REG-ROW does); a build from outside the tree reuses the kernel object an in-tree build made. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-kpath.

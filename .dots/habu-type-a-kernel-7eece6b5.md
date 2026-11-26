---
title: "Type a kernel build by the tree's rows"
status: active
priority: 2
issue-type: task
created-at: "2026-11-26T11:55:20.987650+01:00"
---

Problem: a kernel build checks the boot files against the running hb's rows (src/habu/linker.f OBJECT-ROWS?, PRIMS-IN) and the engine build after it against the tree's (link-kernel.f STATE-PRIMS), so a row whose callers include a boot file (read, realpath, readlink take SPAN:span<u8>: requires.f, include.f) cannot change its types in any number of stages. Acceptance: a kernel build types the tree's files by the tree's rows, and a tree row whose cells differ from hb's is rejected as needing a stage; the span row lanes (dot 31985349) build in one build from an hb with the change; docs/bootstrap.md Staged hosts says so. Depends: none. Claim: agent=jerry workspaces=.jj-ws.noindex/jerry-kcells (the cells check), .jj-ws.noindex/jerry-krl (the rows link).

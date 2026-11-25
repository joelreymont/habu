---
title: Save no build path in a --repl image
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T17:09:40.702882+01:00"
---

Problem: REG-ROW (tools/hb-build-core.f) writes the canonical path of a --repl program's file outside the engine tree into the build's end file, whose APPEND-FILE keeps it in the image, so an image's bytes and its E-UNREQUIRED names depend on the directory it was built in (seen: /tmp/.../app/main.f). Acceptance: two builds of one source from two directories give one image, and a --repl image names its program's files by a path relative to the program's directory; a test in the file-objects suite fails without it. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-repl-path.

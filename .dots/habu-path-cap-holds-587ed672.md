---
title: "PATH-CAP holds Linux paths to Darwin's limit"
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T17:58:10.063181+01:00"
---

Problem: PATH-CAP is 1023 on every target (c453e484), Darwin's PATH_MAX less its NUL, because src/habu/layout.f's per-task FS-ABI and FS-MUT-ABI bands hold PATH-CAP buffers inside USER-REGION-END ($8000). Linux accepts paths up to 4095 bytes, so Habu refuses 1024-4095-byte paths its OS accepts. Acceptance: on each target Habu accepts every path length its OS accepts: PATH-CAP is the OS's PATH_MAX less its NUL (4095 on Linux and Wasm, 1023 on macOS), a test shows the OS refusing PATH-CAP+1 bytes, and FS:MKTEMP-DIR-IN makes a temp under a base that leaves only the temp name's room. Approach (settled 2026-11-13, /tmp/carl-spark/designs/path.md section 10): the header bands are laid upward from USER-BAND's two cells with the path slots last. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-path.

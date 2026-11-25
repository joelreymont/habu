---
title: Keep no scratch row in a program
status: active
priority: 1
issue-type: task
created-at: "2026-11-25T16:28:03.880994+01:00"
---

Problem: every scratch cell costs a program an 8-byte address row (src/habu/link-copy.f KEEP-LIST), though the cell reads zero in every image: a program that uses one 4 KB scratch buffer carries 4064 bytes of rows, and a small program crossed a 64 KB page for it. Acceptance: a program built by the linker keeps no scratch row (or only what a run reads), programs shrink by what they kept, and a run's marks, forks, callbacks and removes act as before. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-rows.

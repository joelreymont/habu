---
title: Give the kept-address rejection its fix
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T16:00:56.347848+01:00"
---

Problem: src/habu/linker.f:1553 REFUSE-CELL (from :1997, reached by every tools/hb-build.f run) prints the cell's space offset and the number it holds, with no source line, no statement that an address made while the build ran was kept, and no fix, against the rejection form (place, source line, expected and found, fix). Acceptance: a build that keeps a heap address in typed storage prints the storage's name and source line, what was found, and the fix (make it when the program starts, or clear the cell before the file ends); test/file-objects.f asserts the text. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-lmsg.

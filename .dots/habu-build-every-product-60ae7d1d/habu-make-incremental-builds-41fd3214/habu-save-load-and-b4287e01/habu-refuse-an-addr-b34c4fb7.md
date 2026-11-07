---
title: "Refuse an address cell the build's body marks past here"
status: open
priority: 3
issue-type: task
created-at: "2026-11-07T04:58:35.866635+02:00"
---

Finding of the address-cell lane's review (2026-11-07): a mark past here made by the build's body, not a file, is refused in the next compiled file's DATA+ (blaming that file), taken as that file's own row if the file lays over it, and kept by nothing when no file follows. Nothing in the tree's build bodies does this today. Fix belongs where the body's marks are seen (the close, or GOING before a file's load): a row at or above the open DP when the next file starts or the build ends is E-COMP-STATE naming the body. Files: src/habu/linker.f, test/fixtures/file-objects/build.f. Claim: unassigned.

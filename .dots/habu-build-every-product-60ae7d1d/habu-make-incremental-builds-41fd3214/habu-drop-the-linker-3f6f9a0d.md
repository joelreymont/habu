---
title: "Drop the linker's REQ-PATH buffer"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T09:14:38.199619+02:00"
---

Problem: src/habu/linker.f LINK-REQUEST copies the path a request spells into REQ-PATH before PARK (REQ-PATH's create, DISPOSE-OBJECTS, INIT-BUFFERS, LINK-REQUEST) because OBJECT used to be overwritten by the requested file's object. Since OBJECT is one buffer per depth, FIELD$ taken before PARK stays valid and can go straight to SOURCE-UNIT:LOAD-PATH (taken after PARK it would read the requested file's buffer). Acceptance: REQ-PATH is gone; a warm engine build equals a cold one. Files: src/habu/linker.f. Verify: engine build cold and warm byte-equal; test/run.f -- file-objects. Depends: none. Ownership: carl. Claim: unassigned.

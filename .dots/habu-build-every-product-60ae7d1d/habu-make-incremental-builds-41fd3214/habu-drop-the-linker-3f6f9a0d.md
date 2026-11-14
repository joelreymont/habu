---
title: "Drop the linker's REQ-PATH buffer"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T09:14:38.199619+02:00"
---

Problem: src/habu/linker.f copies the path a linked request spells into REQ-PATH (:192, disposed at :459, initialised at :483, filled at :4943 and read at :4947) because OBJECT used to be overwritten by the requested file's object. OBJECT is now one buffer per depth, so FIELD$ taken before PARK stays valid and can go straight to SOURCE-UNIT:LOAD-PATH (taken after PARK it would read the requested file's buffer). Acceptance: REQ-PATH is gone; a warm engine build equals a cold one. Depends: none. Claim: unassigned, owner carl.

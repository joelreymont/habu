---
title: "A throw in a build's close leaves the space mapped"
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T13:34:30.724422+01:00"
---

Problem: src/habu/linker.f CLOSE (:5762) repeats UNDO (:5731), whose steps above 'moved' are IMAGE-OUT, DISPOSE-OBJECTS, REQUIRE-RESTORE and CHECK:CLOSE; if one throws, CLOSE ends before the 'moved' step's RESTORE and FORGET-SPACE, and the space stays mapped with hb's state not restored (inferred from the code, not reproduced). Acceptance: a throw in any of those steps still unmaps the space and restores hb's state, shown by a test case that fails on the base. Depends: none. Claim: unassigned.

---
title: Keep the declaration frame API out of user source
status: open
priority: 3
issue-type: task
created-at: "2026-10-25T14:58:51.377678+01:00"
---

TYPE-FIELD-OWNER:OPEN and DECL-EVENT:OPEN run from a top-level user file on a sealed engine (rc 0, carl-int engine 2026-10-25). Their only production caller is DEV-OPEN inside a checker frame; a user-driven field rollback outside a family frame leaves field names in TF-STR (pftx commit, type-family.f frame comment) and UNIT-FAM-TILED then refuses the file for FILE capture. Make the frame API private to the core (whitebox tests keep it), per the privacy rule: private sections plus the build strip.

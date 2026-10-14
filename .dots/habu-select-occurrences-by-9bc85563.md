---
title: Select occurrences by record address
status: open
priority: 1
issue-type: task
created-at: "2026-10-14T22:43:01.268512+03:00"
---

Independent CELL lifetime design inspected current sharedbuild checker RECORD-LINK-OCC: it passes a dictionaryordinal directlyto def-occ-select. Actual native BDEFSELECT validates absolute record address againstDBASE and dividesbyDREC; noordinal canselecttheintendedrecord. Correctcall at exact record-association boundary using actualowningrecordaddress (orcheckeddbase+ordinal*DREC), preserveprimitiveAPI/noname guesses. Covered bycurrentexactoccurrence contract feature; prove representativeprimitive/created/redefinedwordassociation via existingreal record-binding/FILE E2Es, invalidselection unchanged, noimplementationmirrortest. Coreownerengine_shared_core_max implements beforecombinednative/gens/review; primarysource bugnotappworkaround.

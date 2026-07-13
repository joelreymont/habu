---
title: "tools/check: CHK-THROW message shape silently drops its diagnostic"
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T11:46:40.954172+02:00"
---

Found by the scanner-package work (2026-07-13): the existing 's" msg" CHK-E-CHECK CHK-THROW' shape used by the missing-;ENUM / END-VALUE-RECORD arms in tools/check-core.f throws rc 70 with NO diagnostic printed - the message string is dropped on the floor. The new package arms deliberately used CHK-FAIL (print+throw) instead. Fix: migrate the remaining CHK-THROW message sites to CHK-FAIL (or make CHK-THROW print), regression pinning one previously-silent diagnostic's text. Small; tools lane.

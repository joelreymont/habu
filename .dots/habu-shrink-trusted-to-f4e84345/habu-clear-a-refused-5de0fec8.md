---
title: "Clear a refused generates: row's record after it"
status: open
priority: 3
issue-type: task
created-at: "2026-11-01T02:54:28.542471+02:00"
---

Found by the casterr worker: GENERATES-REFUSE (src/core/checker.f) keeps GENR-TOK-A/U after a refused generates: row, so a refusal caught before an image save leaves cells that differ from a build that never ran the row, as CAST-ERROR's record did before CAST-ERROR:DECLARE. tools/check-all-errors-core.f REPORTED-THROW? also lists the ten cast codes CAST-ERROR's private RAISED? lists: one owner should answer which throws a declaration already rendered.

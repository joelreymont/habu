---
title: Store checker effects as typed records
status: active
priority: 2
issue-type: task
created-at: "\"2026-06-28T12:57:56.377908+02:00\""
---

Problem: checker persists certified names/effects as strings in USIGS, so CHECK!/TRUST reparses effects, duplicates are string-shaped, and package scope is encoded in names. Long-term fix: intern stable symbol ids for names, parse signatures once into canonical typed effect records, store certified effects as typed records with diagnostic strings only as metadata, and make TRUST/CHECK! source adapters. Acceptance: certified calls use typed records without reparsing signature strings; duplicate/candidate/package behavior remains correct; existing checker/eval/dictionary gates pass.

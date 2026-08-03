---
title: "Unify the publication seam's record stores"
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T20:38:10.224600+02:00"
---

Four parallel address-keyed record stores now live at the publication seam with the same key and lifetime: the replacement log (publish.f), NCLOB clobber records, NINL body records, and the word-model callable rows. Each grew independently; each has its own capacity, its own refusal, its own growth dot. Unify into one publication record per address carrying all four facts (old/new spans, clobber sets, recorded body, interface), one capacity policy, one write site - net lines negative, all existing refusals preserved by name, every consumer reading through the same readers. Subsumes habu-grow-the-republication-52ef5df0 and habu-grow-the-recorded-f0e9f5da (close both with this).

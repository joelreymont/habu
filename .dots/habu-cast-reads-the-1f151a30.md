---
title: "CAST: reads the next definition off the stream"
status: open
priority: 2
issue-type: task
created-at: "2026-08-19T15:52:11.871693+02:00"
---

Ruling B on the roles.f blocker (1f5980b8's leaf carries the three costed options, 2026-08-19): give CAST: a next-definition window so it delegates to : on the live input stream and needs no evaluate crossing - the same shape NMIGRATE:NEXT landed at master 3e571921 (the engine's own reader decides the extent). This is a checker.f CAST-PEND protocol change. Why B over the others: (A) a second audited evaluate boundary in roles.f converts 34 sites but weakens the single-crossing invariant sumtype.f:923-927 documents as deliberate - a patch; (C) reordering five prefix rows fixes roles.f positionally and leaves every future pre-include.f cast blocked. B also removes the generated-text fidelity caveat for definer-emitted casts. First consumer: roles.f's 34 sites (a converted prefix file cannot build its own fix - land the capability first, sweep second, two steps). Blocked-by nothing; blocks the roles.f half of 1f5980b8.

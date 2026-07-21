---
title: Fail boot loudly on baked-list vs source drift
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T16:31:12.715046+02:00"
---

Structural hazard proven 2026-07-21: when a landing adds a new boot-loaded src/core file (the structure-make split), every EXISTING engine binary on the other machine becomes unable to boot the current checkout - the baked load-row list loads the OLD file set against NEW file contents, dying with a bare E-UNDEFINED (STRUCTURE-MAKE:GENERATE) that looks like a source bug, not a stale-engine condition. Cost ~20 min of diagnosis; healed via a cached candidate binary old enough to predate the reference. Fix at the root: the engine knows its own build identity (engine-id/content key) - at boot, when loading checkout source, detect that the checkout's load-row source (habu2.f PFX rows) disagrees with the baked list and die with a NAMED diagnostic naming the condition and the remedy (rebuild via build-fixpoint install, or bootstrap.sh). A cheap exact check: bake the source hash of the PFX row block; compare at boot; mismatch = loud 'stale engine vs checkout' error. Red-first: an engine built at commit A booting a checkout at commit B with added rows must produce the named error, not E-UNDEFINED.

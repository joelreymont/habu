---
title: "Keep check.f's own types out of the checked program"
status: open
priority: 3
issue-type: task
created-at: "2026-10-01T17:35:26.658182+02:00"
---

Problem: check.f checks a program in-process, so its family registry also holds the families its own require closure declares. `DEFLINEAR outcome` ($HOME/.cache/tmp/kestrel-r4-rev177/p/outcome.f) loads rc 0, but check.f refuses it rc 70 because lib/process.f:19 declares a global `SUMTYPE outcome` that check.f loads and the program does not (not baked into the engine). Today the closure of tools/check.f (46 files) declares only that global row beyond the engine's own NUM NEWTYPEs, so this is the one live collision, but any global family check.f's tooling declares changes what programs it accepts. Found by review 177. Acceptance: a program gets the same verdict from check.f as from the loader whatever check.f's own requires declare: outcome.f admitted by both, and a program that itself requires lib/process.f and then declares `DEFLINEAR outcome` refused by both; decide with evidence whether check.f resets the registry to the engine's baked families before the program, or checks in a child that loads only what the program requires; failing case first in the suite that owns check.f. Base: after deftype c2 (dot 451c6cb6). Files: tools/check-core.f (and wherever the registry scope is set), tools/check-test-lib.f.

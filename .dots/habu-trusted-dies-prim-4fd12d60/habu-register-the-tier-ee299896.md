---
title: Register the tier span capacity exit in engine errors
status: closed
priority: 1
issue-type: task
created-at: "\"2026-09-11T17:23:20.355420+03:00\""
closed-at: "2026-09-16T14:34:51.496451+03:00"
close-reason: "obsolete: TIER-SPAN-FULL does not exist anywhere in the tree and exit 96 is already owned by relocation failures [rg finds no TIER-SPAN-FULL; src/core/engine-error.f:21 assigns 95..99 to SNAP-RELOC]"
---

Owner: Rowan tier lane. src/core/engine-error.f lacks the row for exit96 although habu1.f:1325 and:1356 exit with TIER-SPAN-FULL on frozen f115e7885d20. Add the canonical named engine error entry without changing failure semantics; verify engine error lookup and the explicit capacity refusal. Separate from ordinary-load span exhaustion.

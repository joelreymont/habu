---
title: Census saturates the clobber table at 128 routines
status: open
priority: 2
issue-type: task
created-at: "2026-08-07T11:24:46.404209+02:00"
---

tools/chain-census-core.f drives every plain-colon definition of a scope through held compilation in ONE process, and each definition that compiles publishes a routine. src/compiler/native/clobber.f ROWS-MAX is 128 live published routines and a row is never dropped to make space, so the census saturates it: the 2026-08-07 run reports compiled EXACTLY 128 for lib/ and EXACTLY 128 for src/core+src/habu, with 628 and 64 further definitions refused E-NCLOB-CAP (-8568). That refusal is raised by NCLOB:RECORD-CK inside publish.f VALIDATE-EMISSION, which runs AFTER selection, allocation, register-allocation validation and emission all succeeded - so every one of those definitions compiled completely and was refused only a table slot. The consequence is that the census's compiled count is a CEILING and not a measurement, and any tranche that pushes the compiled count past 128 cannot be read from it. Fix: give the census a way to measure without publishing - either reclaim the routine after each definition (CODE-RECLAIM already exists in src/habu/xref.f and NCLOB drops a row when the code it describes is reclaimed) or add a compile-and-discard mode that stops after regalloc-verify accepts. Until then, report compiled-plus-E-NCLOB-CAP as the compiled count and say so.

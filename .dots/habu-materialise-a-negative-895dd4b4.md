---
title: Materialise a negative constant with one move-wide
status: open
priority: 2
issue-type: task
created-at: "2026-08-02T11:27:37.881363+02:00"
---

src/compiler/native/select.f MATERIALISE always starts with a Movz of the lowest half and adds a Movk per further nonzero half, so -1 costs four instructions and every negative literal costs three or four. ARM64 has Movn, which writes the COMPLEMENT of its half and sets the rest - the engine's own invert uses it (src/habu/habu1.f BINV, 'B 0 MOVN,') - so a value whose halves are mostly all-ones is one Movn plus a Movk per half that is not. Add an a64.movn form (src/arch/arm64/asm.f already has MOVNHW) and let MATERIALISE choose between starting from zero and starting from all-ones by which needs fewer overwrites. Found while landing habu-complete-the-comparison-63760034, where invert was given its own one-instruction machine form (a64.mvn) precisely because the -1 it would otherwise need costs four.

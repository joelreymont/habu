---
title: "Un-respell the old harness's hexadecimal literals"
status: open
priority: 3
issue-type: task
created-at: "2026-08-12T17:23:55.697448+02:00"
---

Full context: tools/codegen-compare-migrated2.f:99 writes SYM-FOLD-C's constants as 65, 90 and 32 where the corpus body (tools/codegen-compare-corpus2.f, copying src/core/checker.f:3542) writes $41, $5A and $20. The file admits the substitution at its lines 28-35 and gives the reason: 'the tape records an integer literal's value by reading the spelling back with the stdlib's decimal reader, and that reader declines a hexadecimal spelling, so the stage refuses the token with E-NFEED-LITERAL'. That reason is gone - habu-record-the-engine-79c570ed replaced the decoder with the engine's own num-parse primitive, and the judge's chain column now compiles the hexadecimal body (test/compiler/judge-baseline.txt CODEGEN-CORPUS2:SYM-FOLD-C, 40 bytes, was -8405 REFUSED). Work: restore the hexadecimal spelling in the migrated body, delete the SYM-FOLD-C entry from the substitution admission at lines 28-35, and re-measure the row rather than assuming the byte count is unchanged (the file's own claim is that the engine compiles both spellings to byte-identical code: WS? 428 either way, SYM-FOLD-C 144 either way - so the old column should not move, and the run is what says so). Not folded into the recording dot because it changes the OLD column's program and that is its own review.

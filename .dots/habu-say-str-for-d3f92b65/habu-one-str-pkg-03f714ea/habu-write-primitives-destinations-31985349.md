---
title: "Write primitives' destinations into a vector"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-23T11:23:37.263334+01:00"
closed-at: "2026-11-26T14:15:11.619213+01:00"
---

Problem: read, readlink, realpath and getdirentries64 (src/habu/prims.f:456, :466, :467, :472) take a ^u8 n the word writes, a row shape Joel ruled out (2026-11-19: str or vector). Approach (Joel 2026-11-26 named >span and span>): SPAN:span becomes a family the checker registers, as str is; identity primitives `>span ( ^a n -- SPAN:span<a> )` and `span> ( SPAN:span<a> -- ^a n )` on arm64, x86-64 and Wasm, mirroring >str and str>; the four rows take SPAN:span<u8>, which VEC:EXTEND answers, and a library word sets the vector's count after the call; SPAN-SPAN:MAKE and UNMAKE go (consumers move to span>), FS:FD-PATH goes; callers 3140c1b7 has not converted wrap their pair in SPAN:MAKE. Design: /tmp/jerry-p1/br/prim-dest-plan.md. Acceptance: no primitive row takes a destination ^u8 n; the four bodies' machine code is unchanged; a str given to read is rejected, expecting SPAN:span<u8>. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-pd.

---
title: Match native at a second does> and a MATCH arm on Gforth
status: open
priority: 2
issue-type: task
created-at: "2026-10-10T00:36:43.797027+03:00"
blocks:
  - habu-hand-the-rest-32631946
---

Problem: review 4 of the Gforth codegen dot (~/.cache/tmp/heron-arm64/evidence/gfcodegen/review-4.md, probes/r4/) found two refusals the host makes differently from native. A second does> with no structure open: the host refuses it at the token on both paths with DIE-DOES's bare `does>`, rc 70 (src/host/gforth/reader.fs DOES-TAKE's DOESB gate, the host's own); native J-DOES has no gate there (habu2.f:4512-4527): its checker refuses a checked body, `E-UNDEFINED habu: in mk: undefined word 'does>'`, and runs the hook (e-dup2-c), and a trusted body compiles, `trusted: MK ( n -- ) create , does> ( -- n ) @ does> ( -- n ) @ ;  5 MK X X . cr` printing 5 (e-dup2-t). does> between MATCH arms: src/host/gforth/codegen.fs CAP-MODE takes the token after ENDOF as a variant without checking it, or that OF follows, so e-marm-c dies `E-UNDEFINED: none` where native prints `hb: match: unknown variant: does> at <path>:<line>`, rc 70.
Acceptance: the host's own second-does> gate goes. A checked body with a second does> is refused by the checker as on native, and a trusted one compiles and runs as native's does. If native's trusted acceptance is unsound (native compiles the second clause wrongly), the worker stops and reports it with file:line, and the lead files a native dot instead. The host checks a MATCH arm's variant against the matched type at the token after ENDOF, with native's message. Cases from e-dup2-c, e-dup2-t and e-marm-c join test/gforth/cases/ and match native.
Files: src/host/gforth/reader.fs, src/host/gforth/codegen.fs, test/gforth/cases/.
Verify: `HB_TMP=$PWD/build/tmp bin/hb --load test/gforth/host-test.f`.
Depends: the hand-the-rest dot (located messages and the exit hook on compile refusals). Ownership: the files above. Worker: worker-max. Claim: unassigned.

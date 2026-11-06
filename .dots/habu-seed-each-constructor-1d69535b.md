---
title: Seed each constructor from its own payloads
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T19:05:19.355707+02:00"
---

Problem: a constructor call takes its type arguments from the first bundle of its family in the declared output when that bundle seeds (src/core/type-family.f TFC-SEED, checker.f CONSTRUCT-DECL-TERM), so a word building two values of one family takes the wrong one for the deeper value. Probe /tmp/optrev.yryF/probes/p16-decl-other.f: `ENUM kk chat embed ;ENUM STRUCTURE pt FIELD x n ;STRUCTURE : P16 ( kk pt -- option<kk> option<pt> ) swap OPTION:SOME swap OPTION:SOME ;` is refused at OPTION:SOME, 'expected: a pt actual: pt kk', on master's checker and on the OPTION:SOME fix (rvtyrpsk, review 2026-11-06 finding 6); the payloads name the right type. Acceptance: P16 compiles and runs; a value whose payload does not name every parameter (RESULT:OK leaves err's free) still takes the declared output's argument for that parameter; the probes in /tmp/optrev.yryF/probes keep their results otherwise. Files: src/core/type-family.f (TFC-SEED, TFC-LIVE-SEED), src/core/checker.f (CONSTRUCT-DECL-TERM), test/type-ctor-suite.f. Verify: build a candidate, test/run.f -- type-ctor type-decl structure-decl enum-decl structure-certify, the p16 probe; full suite (shared checker behavior). Depends: habu-accept-option-some-bd7e9fe0. Ownership: carl. Claim: unassigned.

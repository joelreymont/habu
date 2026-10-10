---
title: Compile a declared return-stack input at tier 1
status: open
priority: 2
issue-type: task
created-at: "2026-10-10T07:08:18.950914+03:00"
---

Problem: a checked or TRUSTED: body whose signature declares a return-stack input is certified by the checker and runs at tier 0, but native tier 1's elaborator refuses it E-NELAB-UNDER (-8304), uncaught, rc 67, with no diagnostic: ~/.cache/tmp/carl-judge/rs/rs1.f (`: RC1 ( | n -- | n ) r> drop 0 >r ;` called under `5 >r`) and rs2.f (`: RP ( | n -- n | ) r> ;`) print `ncomp: cannot compile <name>` and `hb: uncaught throw code -8304`; rs0.f (rs1 at tier 0) prints 0, rc 0. Typed return-stack signatures are language (test/effect-read-api-test.f RETPOP, test/checker-soundness-suite.f TAKE-RU8, test/native-window-loop-obligations.f RETURN-PRESERVE). test/c2-owner-producer-refusals.f REPLACE-R-OWNER stops a tier-1 load of that file the same way (measured by the judge-trusted-shape lane), so its later bodies are never compiled at tier 1.
Acceptance: at tier 1 a body whose declared return-stack input the checker certifies compiles and runs as at tier 0: the elaborator's return-stack vector starts with the declared inputs. rs1 and rs2 join a native tier-1 test with their tier-0 output; a tier-1 load of test/c2-owner-producer-refusals.f passes REPLACE-R-OWNER.
Files: src/compiler/native/ (the elaborator's entry row for the return stack), a native test.
Verify: native build per docs/gate.md; `bin/hb --load test/run.f`.
Depends: none. Worker: worker-max.

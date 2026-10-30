---
title: "r3: full suite on the stacked T0 candidate"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-30T18:41:07.008163+02:00"
closed-at: "2026-10-31T12:12:25.238199+02:00"
close-reason: "Landed with the T0 release (ttloqwsx b17a1ee3): full suite 510/510 on the final candidate"
---

bin/hb --load test/run.f on mnzvrsym's candidate in .jj-ws.noindex/jerry-stack; log /tmp/jerry-big/r3-suite.log. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-stack

2026-10-30 21:00: stack rebased onto master 6dccbb6a and linearized (itick2 into uzllnlko, uref2 into pvovztnu, lanes in one line, imm below ctrl); lead workspace .jj-ws.noindex/jerry-lead on yorztvvy; eim's leftover PROT-EMIT:LCLOSE call dropped (master removed the close with the two-view mapping). Candidate built; full suite on it (ctrl excluded) running, log /tmp/jerry-big/r3b-suite.log. No x86-64 build (see parent). Claim: agent=jerry workspace=.jj-ws.noindex/jerry-lead

2026-10-31 02:40: loc lanes reviewed and folded in; stack (ulzktzrp..sluqzqln, 55 commits) rebased onto master a9fa48b2, two conflicts in tools/native-build-core.f resolved (master captures once: RESTORE-HOST-DISPATCH after CAPTURE). Candidate built in jerry-lead (rc 0); cold prefix boot (aot-wid-build) ok. Full suite running, log /tmp/jerry-big/release-suite.log. Naming: Joel kept every name (/tmp/jerry-big/feature-names.txt). Feature review: Fable at its limit, so Codex Astra (oracle) reviews, output /tmp/jerry-big/oracle-feat/out.txt.

2026-10-31 09:14: full suite on the sluqzqln candidate ran 507 of 507, one red: wasm-link (BODY-SOURCE moved the Wasm image base; fixed in pykovvru, row rc 0). Two-generation convergence on the same candidate: gen 5 matches gen 4 byte for byte (/tmp/jerry-big/two-gen.log). Fable feature review: LAND on what it read; finding 2 -> habu-pin-the-zero-eadd97df; parity read found T0 TRUSTED: miscompiles -> habu-census-t0-s-c0a0be3b. Both block landing; convergence reruns after the T0 fix.

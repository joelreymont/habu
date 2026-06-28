---
title: "Eval grader: isolate candidate checks (namespace per candidate)"
status: open
priority: 2
issue-type: task
created-at: "2026-06-28T12:01:01.569965+02:00"
---

FOR THE CHECKER-OWNING AGENT. This is a core src/core/checker.f change — maki must not edit checker.f. BLOCKING the maki gate (maki/README command, the owning gate for maki changes).

RCA: maki/eval.f CHECK-PASSES? ( = CHECK! -1 = ) checks each candidate IN THE HOST DICTIONARY with NO isolation. Two failure modes, both reproduced with the real self-built bin/hb:
1) ACCUMULATION: candidate words persist across CHECK! calls -> a 2nd candidate with the same word name = 'duplicate definition'. eval-repair.f + eval-fixture.f grade the SAME kernel named K across repair iterations (~16 K-checks in eval-fixture) -> every run duplicates. (Proven: CHECK-PASSES? on "KK (...)" twice -> 2nd is 'duplicate definition: kk'.)
2) CORE SHADOWING: candidate names collide with core words (case-insensitive): a,b,c,k are core (`: a 1 ;` in bare core -> duplicate; d,e free). eval-test.f candidates A/B/C and eval-repair/fixture kernel K collide on the very first check.

MECHANISM (verified call path, self-built bin/hb):
- CHECK! (checker.f:2012) -> CHECK (1977). When a def carries a signature (VSIG@ & SGSEEN@), CHECK calls CHECKER-USIG-CERT-ADD (line 2004).
- CHECKER-USIG-CERT-ADD (1211): CHECKER-CERT-DUP? (1203, = CHECKER-FIND-USIG) -> if the name already exists as a usig, CHECKER-DUP-DEFINITION (1206) dies $4E (=78). Else USIG-ADD appends to the usig registry (USIGS/UEND, 935; UTERM! 1000; truncate via CHECKER-USIGS-TRUNCATE-FROM 1050; HIDE-DEFS-FROM xref.f:157 truncates ndict+usigs together).
- So each CHECK! both (a) PERMANENTLY grows the usig registry and (b) hard-fails if the candidate's def-name already exists. CHECK-RESET (1928) resets analysis state but NOT the registry. (A userland 'ndict@ UEND@ 2>R CHECK-PASSES? 2R> UEND! UTERM! ndict!' wrapper gives rc=70 and still can't stop the in-CHECK! define colliding with core — restore-after is too late.)

KEY CONSTRAINT (why CHECK! itself must NOT be made ephemeral): CHECK! is ALSO the load-checking hook (set-check -> CHECK! in src/habu/snap.f:91 and src/habu/stdin.f:68). During a normal load each definition's usig MUST persist so later code can reference it. So the persistent-register behavior is REQUIRED for compile/load; only the standalone candidate-judge needs ephemeral semantics. A naive "make CHECK! self-contained" fix breaks sequential load-checking — hence a SEPARATE entry, not a change to CHECK!.

FIX (root, new entry — leaves CHECK! semantics intact):
- Add CHECK-CANDIDATE! ( ptr u8 n -- verdict ) to src/core/checker.f with EPHEMERAL + SHADOW semantics: snapshot UEND@ before; set a flag (e.g. CHK-CAND) so CHECKER-USIG-CERT-ADD SUPPRESSES CHECKER-DUP-DEFINITION (let the candidate shadow existing usigs, incl. core); run CHECK; clear the flag; restore the registry (UEND ! ; UTERM!) so nothing persists (mirror CHECKER-USIGS-TRUNCATE-FROM at 1050). Net: a candidate of ANY name checks once or repeatedly with zero persistence/collision; the engine's normal load-checking + duplicate detection are untouched.
- maki/eval.f: switch CHECK-PASSES? from CHECK! to CHECK-CANDIDATE! (one-line maki change once the core entry exists).

VERIFY: eval-test/eval-repair/eval-fixture grade green; a candidate literally named 'k' or 'a' certifies; CHECK-PASSES? on the same name twice both succeed; the host usig registry is byte-identical before/after a candidate batch (no growth); native gate green (CHECK! unchanged); full maki gate green.

NOTE: master native gate (test/run.f) is green and does NOT include this maki gate, so master looks green while the maki owning gate is red. The eval-grader stopgap (CP-REWRITE auto-rename) was reverted in favor of this core fix per the user; maki/eval.f currently still uses CHECK! (no maki-side hack) and stays RED until CHECK-CANDIDATE! lands. Blocks FF-ing branch maki-autograd (SUB+SQUARE) to master.

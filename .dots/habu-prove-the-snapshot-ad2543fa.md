---
title: Prove the snapshot relocation round trip
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-30T09:22:08.248509+02:00\""
---

Full context: PRIORITY 1 proof upgrade, directed 2026-07-30. The costliest defect class of this campaign - the deleted BL relocation pass, the four-value displacement lottery, stale persisted cells - all violated one unstated invariant: for every RECORDED site, writer-side canonicalization composed with loader-side rebase is the IDENTITY, for any writer and loader bases within BL reach. Nothing states or checks that today. Build the model: formal/Common/Reloc.v with the BL imm26 arithmetic (sign-preserving shift, 26-bit mask, the >>2 instruction units), the canonical forms (call displacement as if the region sat exactly REGION-OFF above text; declared DATA cells relative to the RBASE-VA sentinel - read src/habu/habu2.f SNAP-RELOC:EMIT-CALLS/EMIT-XT and src/habu/snap-lib.f SND-CANON-XT-CELLS for the shipped arithmetic), and theorems: round-trip identity for recorded sites under arbitrary base pairs within reach; region-internal displacements untouched; a non-call word at a recorded site is refused (the CALLMAP-RC arm), never rewritten. Binding to the shipped code is the hard half and must be honest: the pass is emitted assembly, so either (a) extract the displacement arithmetic into checked Habu words the emitter test drives with the SAME shared vector rows that generate the Rocq obligations (parity-gate style, one table), plus a mutation proof that skewing the emitted pass (shift constant off by one in EMIT-CALLS) reds a real write-restore boot test; or (b) if extraction is infeasible, the vector table drives a real snapshot write/restore on synthetic recorded sites and the dot records why. Never a model that only talks to itself - AGENTS.md Proof Integrity worth test applies: a plausible change to the SHIPPED arithmetic must falsify the gate.

Claim: agent=relocproof workspace=.jj-ws/habu-prove-the-snapshot-ad2543fa

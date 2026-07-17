---
title: "Checker: cp.async pipeline typestate capability"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-16T19:10:50.469826+02:00\""
---

The long-term closer for the pipelined-GEMM typed vocabulary (habu-typed-pipelined-register-4d20acb5): make the checker EXPRESS the cp.async pipeline discipline so the staging bodies stop being trusted. Today the tilepipe vocabulary encodes what the type system can already state fail-closed (nominal families: buffer-parity types, alignment obligations, layout-parameterized tiles, pipelined-tile distinct from scalar tile) but the DYNAMIC pipeline protocol - cp.async.cg.shared.global issue -> commit_group -> wait_group N -> bar.sync -> read staged data, with double-buffer parity alternation and no read-before-wait - is enforced only by the proven emitter bodies (named TRUSTED boundaries owned by THIS dot). Work: a checker typestate/linear capability modeling the pipeline: staged-buffer tokens minted by the async-copy word in state pending<parity>, consumed exactly-once through commit/wait into state ready<parity>, reads require ready of the matching parity, bar.sync transitions per the M5 barrier model (habu-ptx-m5-mask-eb0716f1 - block-uniform reachability; compose, do not duplicate), loop-carried parity alternation provable across TILE-LOOP iterations (relates to the linear-once capability habu-linear-once-resource-4c58a7a1 and the loop-carried row machinery). Negatives: read-before-wait rejects; parity mismatch rejects; missing commit rejects; double-wait rejects. Acceptance: the tilepipe staging words' bodies re-expressed as CHECKED code certifying under the new model, byte-identical PTX preserved (capture+cmp), the TRUSTED rows this dot owns REMOVED from TRUSTED.md, trusted-inventory ratchet down, all ptx suites + device goldens (on zed return) green, negative fixtures for each reject. Files: src/core/checker.f + type-family machinery (COORDINATE: tfam sealed-packages lane), lib/ptx staging words, tests. Ownership: checker capability - the stored-xt/trust-retirement program's kernel-side sibling. Blocks nothing today (tilepipe lands with named boundaries); this dot's landing is what deletes them.

FOLDED IN 2026-07-16 (GEMM stage-3 landing): remove the now-unreferenced
mmacc TFAM row from src/core/type-family.f when this dot's capability work
re-touches the registry (core edit + fixpoint rebake; deliberately excluded
from the lib-only stage-3 commit). mmctx remains referenced.

Claim: agent=cpasync workspace=.jj-ws/fable-cpasync (host lane - checker typestate; NO zed access, byte-identical PTX is the device-equivalence proof)

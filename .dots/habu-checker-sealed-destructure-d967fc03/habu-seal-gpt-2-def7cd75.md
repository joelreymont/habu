---
title: Seal GPT-2 layer identity
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T20:50:54.876199+02:00"
blocks:
  - habu-publish-make-only-a40591e2
---

Problem: GPT2TENSOR:layer-id exposes public UNMAKE, allowing a holder to reuse layer-proof and a config key around an arbitrary layer index and weakening GPT2TENSOR:E-CONFIG identity. Required result: declare layer-id with DESTRUCT owner, migrate only its original-block inverse uses to destructure layer-id, retain LAYER-ID as the sole proof mint, and let ;package seal GPT2TENSOR. Remove the stale caveat. Do not change tensor-role enums, slot arithmetic, name rendering, shape, orientation, or error codes. The existing GPT2TENSOR-TEST package remains black-box; no reopen or test friend is permitted. Owner: maki/infer/gpt2-tensor.f and its existing suite only. Dependency: habu-publish-make-only-a40591e2. Acceptance: external proof/key/index reconstruction and a package-reopen variant flip from ACCEPT to checker rejection; LAYER-ID, GPT2TENSOR:SLOT, GPT2TENSOR:COPY-NAME?, shape, and orientation tests remain unchanged; foreign-config and out-of-range cases still reject at their named production checks; no public projection exposes layer-proof; focused package, typed-local, signature, and trust gates pass.

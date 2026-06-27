---
title: "Autograd: transformer-block VJP coverage (matmul/attention/layernorm/GELU/residual/embedding)"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T23:00:02.511319+02:00"
---

Cover the ops a real MLP+attention block needs, each with forward + VJP + gradcheck: MATMUL-BWD (dA=dC.Bt, dB=At.dC - the two GEMMs), ATTENTION-BWD (flash backward: recompute scores, dV/dK/dQ), LAYERNORM fwd+bwd, GELU fwd+bwd, RESIDUAL (add - trivial), EMBEDDING (gather fwd / scatter-add bwd). Each lowers onto the checked Habu-PTX kernels (cg-matmul, cg-attention) and is device-gradchecked. Files: maki/autograd.f + lib/ptx kernels + ad.f VJP-EXPAND entries. VERIFY: each op's analytic VJP matches central finite difference within tol on the Orin. Dep: EPIC; relates habu-ad-softmax-rows, habu-ptx-ad-device, habu-re-express-tiled.

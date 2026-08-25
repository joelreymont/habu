---
title: Pin Qwen reference outputs
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.446621+02:00"
blocks:
  - habu-infer-dense-tensor-c037a6fd
---

Why this exists:
a reproducible external oracle must be frozen before any Qwen device kernel is accepted.

Required result:
add data-only package QWEN-REFERENCE. It stores the exact prompt identifiers, 64 continuation identifiers and decoded-byte digest, selected BF16 stage probes, RoPE lane probes, and stable logit top-ten rows below. It contains no forward executor, tokenizer, weight reader, tolerance policy, or runtime reference generation.

The oracle is Qwen/Qwen2.5-7B-Instruct revision a09a35458c702b33eeacc393d103063234e8bc28 under Transformers 4.43.1 eager with explicit DynamicCache and 39 sequential one-token prefill calls. Environment: Python 3.12.13; torch 2.11.0+cu130 commit 70d99e998b4955e0049d13a98d77ae1b14db1f45; tokenizers 0.19.1; CUDA 13.0; GB10 sm_121; deterministic algorithms; CUBLAS_WORKSPACE_CONFIG=:4096:8; matmul TF32 false; BF16 and FP16 reduced-precision reduction true. Greedy ties use stable lower-identifier order and scan only [0,151665). Two fresh-cache runs matched all identifiers, decoded bytes, 64 complete 152064-entry FP32-logit hashes, stable top-ten rows, and 655 captured vectors bitwise. The external capture JSON SHA-256 is 8dcb80b439e5faad0205638ce39016428548c7b87c7bc1c0b5b5c030dffd1a56; its one-time runner SHA-256 is ccc697142ff31fca29a5546b3014b70c34e5ba900f99563c6590ef8a623e1949. Neither is committed or run by Habu.

The exact 201-byte prompt SHA-256 is 0a97b564a2e9a08850a57f58b4570e43d46b9ac997a97038f754abe34e708269 and its identifiers are:

151644 8948 198 2610 525 1207 16948 11 3465 553 54364 14817 13 1446 525 264 10950 17847 13 151645 198 151644 872 198 35127 752 264 2805 16800 311 3460 4128 1614 13 151645 198 151644 77091 198

The exact 64 continuation identifiers are:

32 3460 4128 1614 320 4086 44 8 374 264 943 315 20443 11229 1614 6188 311 3535 323 6923 3738 12681 1467 13 4220 4119 525 11136 3118 389 5538 6832 12538 11 7945 42578 77235 11 323 525 16176 389 12767 14713 315 1467 821 504 279 7602 11 6467 11 323 1008 8173 13 444 10994 82 646 2736 264 6884

Their decoded bytes have SHA-256 32646139d1ccab90dc8f6a13ca78e4caf6952c808f433ff7d330e20a7a573f83 and end with `LLMs can perform a wide`. Final cache length is 102.

At absolute position 38, selected BF16 first values are: embedding 0.00145721435546875; layer-0 input RMSNorm 0.025634765625; raw Q/K/V 0.0252685546875, -1.0390625, 0.017578125; post-RoPE Q/K 0.259765625, -0.02734375; pre-O attention 0.0216064453125; O output -0.034423828125; post-attention RMSNorm -0.076171875; SwiGLU/down input -0.024169921875; layer outputs 0/13/27 -0.1025390625, -0.73828125, 2.78125; final RMSNorm 1.828125. Layer-0 input-RMSNorm summary is count 3584, sum 11.274537563323975, sumsq 251.32989086877865. The position-38 valid-logit top ten are 32:33.25, 39814:33, 95456:32.75, 34253:31, 101951:23.25, 2124:22.625, 20286:22.625, 2704:21.875, 19098:21.5, 77045:21.5. Selected logits include 0:1.28125, 1:9.125, 151643:-6.53125, 151644:4.90625, 151645:3.875, 151664:2.375, and padded 152063:2.375.

The data module also stores the complete BF16 vectors, not summaries, for position 38 input RMSNorm, Q, K, V, post-RoPE Q and K, attention output, O/residual, gate, up, SwiGLU, down, layer outputs 0, 13, and 27, final norm, and the complete 152064-entry BF16 logit row before widening. Each vector has its exact role, layer, position, length, and digest. Sparse scalar probes remain indexing checks only; sums, norms, top rows, and hashes cannot substitute for elementwise kernel acceptance.

For layer-0 head 0, each Q/K row below is lane order 0,64,63,127: position 0 Q [0.4921875,-0.003082275390625,0.056396484375,-0.64453125], K [-0.55859375,-4.4375,171,-114.5]; position 1 Q [0.11669921875,0.24609375,0.234375,-0.466796875], K [3.609375,-2.859375,171,-113.5]; position 15 Q [-0.08984375,0.640625,0.73828125,-1.484375], K [2.546875,2.625,171,-115]; position 16 Q [-0.267578125,-0.109375,0.69921875,-0.87109375], K [-0.4296875,4.21875,171,-115]; position 38 Q [0.259765625,-0.74609375,-2.390625,-0.291015625], K [-0.02734375,-3.40625,172,-114.5]; the same raw position-38 inputs rotated at position 32767 yield Q [0.1728515625,-0.7734375,-2.375,-0.388671875], K [-0.40625,-3.375,177,-107.5].

Done when:
the data module returns every exact value and complete vector above, its tests pin the full prompt and continuation arrays plus decoded-byte digest, and mutating any stored identifier, role, layer, position, lane, length, vector word, or scalar fails. Kernel dots compare their named complete outputs element by element with this oracle; every pinned sparse word is bit-equal, every complete BF16 element is within one adjacent BF16 representable value, NaN or Inf mismatch fails, widened FP32 logits equal widening of the accepted BF16 values, and valid-domain argmax is exact. Aggregate sums and BF16-to-FP32 drift cannot satisfy acceptance.

Expected touch points: new maki/infer/qwen-reference-data.f and focused data test only.
Smallest check: bin/hb --load maki/infer/qwen-reference-data-test.f.
Prerequisites: pinned Qwen artifact and tensor roles.
Owned result: one immutable external-oracle fixture only.
Claim: unassigned.

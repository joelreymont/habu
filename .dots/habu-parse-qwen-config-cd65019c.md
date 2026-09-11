---
title: Parse Qwen configuration
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T22:00:35.832115+02:00"
blocks:
  - habu-add-qwen-model-bf23d2ff
  - habu-infer-dense-pin-36c8e45c
  - habu-own-model-asset-c6f938e4
---

Why: the production loader must obtain the Qwen row from the pinned config through the sole normalized parser. Interface: HFCFG:OPEN-QWEN takes and returns MODEL-ASSET:ws with a root ptr u8 plus CAD-NUM:byte-len, authenticates the exact QWENPIN config asset once, then returns the existing parse result after consuming exactly model_type=qwen2, architectures, hidden_size, num_hidden_layers, num_attention_heads, num_key_value_heads, intermediate_size, max_position_embeddings, rms_norm_eps, rope_theta, hidden_act=silu, torch_dtype=bfloat16, vocab_size, tie_word_embeddings=false, bos_token_id 151643, and scalar eos_token_id 151645. The file omits head_dim, attention_bias, and mlp_bias; derive head size through MDLCFG and reject any occurrence of those unsupported keys. Unknown fields are ignored only after digest match. Every result arm returns the workspace. Unsafe or overlong root and missing, duplicate, wrong-type, inconsistent, or unsupported required values reject before config publication. QWENPIN owns the ordered stop and pad constants. Owner: maki/infer/hf-config.f qwen2 parse branch only. Production red: pinned config bytes cannot produce MDLCFG:mcfg. Acceptance: the pinned root yields the exact row; digest, hostile structure, head_dim or bias presence, EOS list, and semantic mismatch fixtures exercise the real file-open parser with workspace ownership intact; GPT-2 stays exact; the asset is opened and hashed once. Forbidden: package-global path or file buffer, DOM, verified-root token, schema, default for a required field, model-name heuristic, optional field, fallback, or compatibility parser. Smallest owning check: bin/hb --load maki/infer/hf-config-test.f. Claim: unassigned.

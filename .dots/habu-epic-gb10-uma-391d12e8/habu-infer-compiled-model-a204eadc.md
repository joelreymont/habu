---
title: "Infer: compiled model-pack format"
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-21T16:44:32.519588+02:00\""
blocks:
  - habu-infer-safetensors-loader-0b58e06a
---

Plan-of-record sect 5.1: no launch rediscovers or repacks the model. Define the pack: manifest, normalized config, tokenizer assets, packed weights (packer owns transposition/swizzle/block-scaling/alignment/naming), layouts, quantization record, kernel/schedule keys, quality + benchmark records, checksums. Runtime loading becomes deliberately boring: map, verify checksums, go. Pack creation is the habu pack command's core; never both full models resident during packing (bounded chunks). Red-first: checksum mismatch, version skew, truncated member each fail closed named before any registration.

Claim: agent=fable-modelpack workspace=.jj-ws/fable-modelpack machine=spark

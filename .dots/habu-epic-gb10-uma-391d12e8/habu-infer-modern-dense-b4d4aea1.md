---
title: "Infer: modern dense model family"
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T16:44:54.193349+02:00"
blocks:
  - habu-infer-dense-safe-3b25bdfa
---

This is the modern dense-model campaign record. Do not dispatch it as implementation work. Its leaves pin one checkpoint and tokenizer, bind its tensors and configuration, build the host reference block, integrate grouped-query attention and the vocabulary head, prove full BF16 continuation parity, and enforce the measured memory boundary. The campaign closes when the safe-boundary leaf lands.

---
title: Tiny-shakespeare char tokenizer + text data loader (v0)
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-18T15:24:38.483451+02:00\""
---

ABSENT: no tokenizer/vocab/text pipeline; all datasets are synthetic LCG (from-scratch-model.f). Add the v0 char tokenizer: build vocab from the corpus (unique chars sorted), encode text->ids, decode ids->text, and a get_batch that draws contiguous (x,y) token-id windows (y = x shifted by one) as the RxC row buffers the model consumes. Habu-native file read for the corpus. Dep: none (feeds the training loop).

Claim: agent=shakespeare workspace=.jj-ws/shakespeare machine=spark

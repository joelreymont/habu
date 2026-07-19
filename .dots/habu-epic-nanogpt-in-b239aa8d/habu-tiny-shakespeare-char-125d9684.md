---
title: Tiny-shakespeare char tokenizer + text data loader (v0)
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-18T15:24:38.483451+02:00\\\"\""
closed-at: "2026-07-19T22:34:42.800201+02:00"
close-reason: "Landed eec1609b-era: maki/tokenizer.f (TOK-BUILD stable sorted vocab, encode/decode round-trip, E-TOK-EMPTY/-RANGE/-CAP) + maki/data-loader.f (DL-LOAD-CORPUS via checked lib/fs READ-ALL, E-DL-EMPTY). get_batch correctly REUSES existing maki/batch-loader.f BL-LOAD (seeded LCG windows, y=x>>1, BxT rows) - only the text->ids half was missing. Id contract: one id per cell as float repr, matching EMB-GATHER's T-GET f>s. No corpus committed; inline Hamlet fixture + real fs round-trip via /tmp"
---

ABSENT: no tokenizer/vocab/text pipeline; all datasets are synthetic LCG (from-scratch-model.f). Add the v0 char tokenizer: build vocab from the corpus (unique chars sorted), encode text->ids, decode ids->text, and a get_batch that draws contiguous (x,y) token-id windows (y = x shifted by one) as the RxC row buffers the model consumes. Habu-native file read for the corpus. Dep: none (feeds the training loop).

Claim: agent=shakespeare workspace=.jj-ws/shakespeare machine=spark

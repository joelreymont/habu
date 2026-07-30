---
title: "Infer: GPT-2 pinned inputs"
status: open
priority: 1
issue-type: task
created-at: "\"2026-07-21T15:57:56.745951+02:00\""
blocks:
  - habu-own-gpt-2-664626a8
---

Campaign only; do not dispatch. Own the exact GPT-2 checkpoint identity, reusable byte-pair encoding state, and GPT-2 tokenizer adapter consumed by the shared device engine. The landed GPT2-REFERENCE package already owns the correctness probes, logits, and 64 reference identifiers; no second forward implementation or reference generator belongs here.

Close when the pinned config, safetensors, vocabulary, and merges are exact, two tokenizer owners interleave, and the committed prompt encodes and decodes byte-exactly. Direct GPT2DEV loading and all computation remain in the persistent executor and INFER campaigns. Rejected host-forward commit 736a887c and the removed host-forward dots remain evidence at source commit 85a9646fd6b97e5d2cbb86d637bcf8d8ab2aece8 and must not land.

Claim: unassigned.

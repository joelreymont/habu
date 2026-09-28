---
title: Encode effect links as record spans
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-09-28T16:16:13.808669+02:00\\\"\""
closed-at: "2026-09-28T16:51:22.103046+02:00"
close-reason: Encode local NEXT as record-relative cell spans while preserving every record, byte offset, history horizon and stable wire endpoint. Exact reviewed source dfe01e6c; engine 2790775 -> 2741239 bytes (-49536), DATA value section -41964, AOT code +76, net payload -41880 before padding/signature. B1-B5 and names identical; full 492-suite gate, focused tests, independent review, strict signatures and actual Maki stdin smokes pass. Maki -32832 bytes; both boards byte-identical. Engine SHA256 6c9d991160f61a0d08a4c82ebfba1e5b80745d45b7b9fb980aa8f78bbf54bcad. Evidence ~/.cache/tmp/habu-effect-links-fix-20260928-01/ and habu-effect-links-review-20260928-01.md.
---

The current 40-byte local effect bindings store ER.NEXT as an absolute USIGS arena offset. Exact ad77f4fd engine SHA ae41d43ac92b69d6c0a1bc45b92f4f359a77b7766fd991e2fa4930e10b83ca02 has 21,071 bindings costing 63,081 NEXT value bytes. Byte spans would cost 21,926; aligned cell spans 21,097, saving 41,984 before compiler/framing cost. Every span is 8-aligned and between 40 and 5,408 bytes; the chain reaches UEND 1,123,336. All 105,355 header fields reconcile to the restored live prefix. E-REC-FINISH publishes the end of each binding and its newly owned graph/content span. Use cell spans without deleting or moving records; measure complete-product cost before acceptance.

Measure byte-span and cell-span encodings on the exact captured image, validating alignment, strict forward chain, UEND and zero terminator. Establish all owner/raw-walker/wire consumers before selecting the representation. Preserve every raw record position, binding identity, authority, history horizon, content/node reference, zero unfinished state, rewind/regrow and growth behavior. Stable 96-byte owner/payload wire fields must retain their existing meaning.

Own src/core/checker.f local NEXT readers/writer and existing effect-store census boundary plus genuinely affected semantic E2Es. No generic snapshot codec, history pruning, symbol pruning or NORET expansion. Tests for uncovered behavioral gaps precede code; reuse rollback, effect interning, source reconstruction, owner transfer and saved-image E2Es. Require measured total payload benefit including code cost, native generations and names identity, full test/run.f, actual Maki stdin/board equivalence and signatures. Lead owns dots/integration/closure; Sol owns implementation after bounded design and measurement. Current design/census receipts are habu-effect-link-design-20260928-01.md and habu-effect-link-census-completion-20260928-01.md under ~/.cache/tmp.

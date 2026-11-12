---
title: "Measure OpenAI's refusal of an embed request over its token limit"
status: open
priority: 2
issue-type: task
created-at: "2026-11-04T19:09:40.472550+02:00"
---

Problem: MODEL's size-refusal test (SIZE-MEMBER? src/lib/net/model.f:1971, SIZE-REFUSAL? :2037) recognizes context_length_exceeded, string_above_max_length and the text "This model's maximum context length". Public reports (unmeasured here) say OpenAI refuses an embeddings request whose inputs sum past 300,000 tokens with 400, code max_tokens_per_request, message 'Requested N tokens, max 300000 tokens per request'. If so, EMBED answers E-HTTP where E-CAPACITY's comment (model.f) promises E-CAPACITY for a request refused for its size. A caller reaches it with MODEL:INPUT-MAX at 2048 when it does not group inputs by MODEL:TOKENS-PER-EMBED-MAX; Zen groups, so Zen does not reach it. Acceptance: one live EMBED over 300,000 tokens records the provider's status and body (cost about 0.006 USD at text-embedding-3-small); if the code differs from those SIZE-MEMBER? takes, add it there with a peer case in src/lib/net/model-test.f (EMBEDDINGS' too-large test shape) that fails E-HTTP before and E-CAPACITY after. Depends: none. Claim: unassigned.

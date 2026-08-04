---
title: "Preserve GPT-2 byte grammar"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-23T09:47:20.909072+02:00\""
blocks:
  - habu-bpe-unicode-data-45a7c2e9
  - habu-bpe-utf8-scalar-8c1d6f34
---

Why: model-owned `GPT2:T-CP@` erases the tagged `UTF8:NEXT` distinction between a valid scalar and one malformed raw byte. The class matcher can therefore treat an invalid lead such as `C3` as Unicode U+00C3 Letter and merge it with adjacent ASCII. Result: inside the existing private `GPT2` tokenizer, use shared `UTF8:NEXT` and retain its scalar/raw-byte arm through every grammar decision. Scalars use `UNICODE-CLASS`; every raw byte is exactly one byte of progress and always OTHER, never Letter, Number, or White_Space. Preserve the existing leftmost greedy GPT-2 alternatives and arbitrary-byte round trips. Delete `T-CP@`; do not add another decoder, tokenizer package, public splitter, test hook, table, alias, manifest, lint, framework, or compatibility path.

Owner: `maki/infer/gpt2-token.f` private chunk classification and its production-path tests only. Source evidence to adapt, not merge: `602f4566da0a7b981f8cf6000f96deea49b87609`. Dependencies: landed `UTF8:NEXT`, `UNICODE-CLASS`, and model-owned tokenizer. This result does not own workspace capacity, vocabulary data, tokenizer lifetime, training, or a public grammar API. Production red: real `GPT2:ENCODE` currently joins malformed `C3` with following ASCII as a Letter run.

Acceptance: real model-owned encoding pins ASCII adjacency and every contraction; fullwidth and astral Letter/Number boundaries; combining marks and punctuation; all 25 Unicode White_Space scalars including multiple-scalar tail retention; and malformed vectors M1-M9 covering bad continuation, invalid/overlong lead, truncated 3/4-byte sequences, stray continuation, surrogate, above U+10FFFF, bad final continuation, and overlong pair. Representative malformed, fullwidth, astral, and whitespace cases pass through real `GPT2:ENCODE` and exact decode round trips. Seven production-path guard-page cases put terminal apostrophe and each truncated 2/3/4-byte sequence immediately before an unmapped page and do not fault or overread. Mutating the tagged arm, raw-byte class, alternative order, or whitespace backtracking fails the owning tests. Smallest owning-path check: load the pinned model and encode `C3` followed by ASCII through the real private `GPT2:ENCODE` entry used by `GPT2:GENERATE`; it must retain the raw-byte boundary and round-trip exact bytes. Files: `maki/infer/gpt2-token.f`, `maki/infer/gpt2-generate-test.f`, and the smallest guard child/test needed for unmapped-page proof. Run the focused model/generate, typed-local, package, and canonical gates. Claim: agent=gpt2_unicode_fix workspace=.jj-ws/habu-bpe-install-unicode-3c84e7a1-r2.

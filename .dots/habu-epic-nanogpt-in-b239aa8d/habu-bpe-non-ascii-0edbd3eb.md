---
title: "BPE: non-ASCII whitespace class"
status: open
priority: 3
issue-type: task
created-at: "2026-07-21T11:32:43.588026+02:00"
---

Follow-up from the unicode pre-split closure (531cab6e): the matcher's whitespace class is deliberately still ASCII-only - the GPT-2 regex's backslash-s includes unicode spaces (U+00A0 nbsp, U+2000-200A, etc.), so text containing them can chunk differently from tiktoken. Same bounded-table treatment as Letter/Number: generate the White_Space ranges with provenance, extend the class predicate, fixture flips red-first, id-level residual scan for the honest boundary. maki/examples/nanogpt/bpe*.f.

Claim: agent=bpews workspace=.jj-ws/fable-bpews machine=spark (owns the non-ASCII whitespace class for the BPE pre-split)

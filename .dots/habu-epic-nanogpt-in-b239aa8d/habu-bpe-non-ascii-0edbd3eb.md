---
title: "BPE: non-ASCII whitespace class"
status: closed
priority: 3
issue-type: task
created-at: "\"2026-07-21T11:32:43.588026+02:00\""
closed-at: "2026-07-21T13:28:50.126103+02:00"
close-reason: "Landed 149b4ca8: the tokenizer's whitespace class is now unicode-complete - the LAST pre-split divergence class is closed. The predicate classifies by codepoint against the FULL 25-codepoint Unicode White_Space set (generated with provenance, cross-checked 25==25 against tiktoken's compiled class; only 128 bytes of table since whitespace needs no block-bounding), and the whitespace tail was structurally fixed to leave the last WHOLE codepoint for the next chunk - the old byte-based tail split multi-byte spaces mid-sequence, proven load-bearing (fixtures fail with the predicate alone). Red-first with exact tiktoken chunk vectors covering 2- and 3-byte spaces and the control-set derivation; the former ASCII matcher had 13 measured real id divergences, now ZERO across all 63456 relevant codepoints in exhaustive probes. The honest proof note: no subset-vocab fixture can be red-first for a splitting change (the base over-folds reproduce tiktoken THROUGH the subset - confirmed empirically), so parity rests on matcher byte-vectors + the residual scan, documented in the header. Full tests green at the merged tip"
---

Follow-up from the unicode pre-split closure (531cab6e): the matcher's whitespace class is deliberately still ASCII-only - the GPT-2 regex's backslash-s includes unicode spaces (U+00A0 nbsp, U+2000-200A, etc.), so text containing them can chunk differently from tiktoken. Same bounded-table treatment as Letter/Number: generate the White_Space ranges with provenance, extend the class predicate, fixture flips red-first, id-level residual scan for the honest boundary. maki/examples/nanogpt/bpe*.f.

Claim: agent=bpews workspace=.jj-ws/fable-bpews machine=spark (owns the non-ASCII whitespace class for the BPE pre-split)

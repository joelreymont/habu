---
title: Add integer token embedding rows
status: active
priority: 1
issue-type: task
created-at: "2026-07-28T19:27:40.373420+02:00"
---

Why: forward decode produces integer token ids, but embedding lookup today reads ids out of a float tensor (EMB-GATHER/TOKPOS-EMBED in maki/embedding.f), forcing an integer-to-float-to-index round trip on the inference path, and the current path provably accepts an out-of-range id (probe: token id 99 against a 3-row table reads out of bounds). Exact result: public MAKI:EMBED-ROWS ( ptr a ptr ptr a n n n -- ) in maki/embedding.f - wte buffer, integer id cell buffer, destination buffer, row count to gather, embedding dim, vocab row bound. Every id is bounded against the vocab row count BEFORE any read; an out-of-range id throws the file's new named code E-TOKEN-RANGE -5179 (mirroring the E-WPE-EXTENT -5169 convention); in-range ids copy their wte row to the destination. Loader-independent: raw MAKI compute-buffer style like MATMUL, no GPT2LOAD, WSTORE, or model-config contact. The existing float-id TOKPOS-EMBED path is untouched; its convergence with this word is recorded debt for the dot that unifies training and inference embedding. Owner: package MAKI in maki/embedding.f. Acceptance: exact row-copy values on a hand-pinned table; boundary ids 0 and nvocab-1 accepted, nvocab and negative rejected with E-TOKEN-RANGE and an untouched destination sentinel; embedding and autograd suites green unchanged; both diff lints. Forbidden: clamping, float id conversion, touching TOKPOS-EMBED behavior, loader types.

Claim: agent=claude workspace=.jj-ws/habu-add-int-token-75a152ee

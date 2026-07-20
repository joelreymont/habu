---
title: Pin factored MATCH stencils
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-19T22:27:54.853603+02:00\\\"\""
closed-at: "2026-07-20T10:47:17.508021+02:00"
close-reason: "Landed bd23829a: test/match-factor-pin.f pins exact emitted bytes for 5 representative words (verbatim hex spans), every factored failure diagnostic (construct/match unknown, expected-of rc70, bad-mfp-tag rc85, underdepth), MATCH success + W^X round-trip, package wordlist restoration, and a load-scaled compile-throughput ratchet. Both-direction: fixture green on a rebuilt pre-factoring engine AND the factored one - byte/character identical, so future emit drift reds the gate without a historical engine. Throughput A/B: 500 compiles x15, factored 212.2ms vs unfactored 213.3ms median - null result, shared-BL overhead unmeasurable"
---

Current master verification gap after ed6207a00ef7 factored native MATCH compiler text: the landing changed src/habu/habu2.f and size attribution but committed no focused regression for LADTPUSHTOK, LMFRTOP, LADTDIE, or the factored failure branches. The factor is a measured 408-byte compiler-text win, but its required byte-identical emitted-user-code and diagnostic-path proof is not persistent. Add a focused checked fixture covering payload construction, MATCH success, tag mismatch, underdepth, wide payloads, package wordlist restoration, write-xor-execute discipline, ahead-of-time compilation, and fixpoint bootstrap. Pin exact emitted bytes and disassembly for representative positive and negative programs before and after factoring; prove only compiler implementation text changes and every diagnostic remains exact. Add a representative large algebraic-data-type compile-throughput benchmark so shared-call overhead is measured, not assumed. Coordinate unreachable large-tag removal with habu-remove-unreachable-match-66326749 and broader compiler-name factoring with habu-share-compiler-name-1ecfb58c; this dot owns the persistent MATCH factoring regression and its size-speed evidence.

Claim: agent=matchpin workspace=.jj-ws/fable-matchpin machine=spark (TEST-ONLY lane: adds the persistent MATCH factoring fixture; src/habu/habu2.f is NOT edited, so no CODELEN row movement)

---
title: Respell authored equations in the canonical pretty form
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-20T10:01:26.116912+02:00\""
---

Joel 2026-07-20: make the code use the beautiful new SPEC/MODEL syntax. docs/golden-syntax.md:187-193 fixes ONE canonical pretty form - prefix summation, infix product, real Unicode glyphs: O[m n] = Σk A[ix[m] k] · B[n k] - and reserves the ASCII spelling for byte-oriented Forth tests that must not carry multi-byte bytes. Yet EVERY authored SPEC: line in the tree still uses the ASCII trailing form (* +SUM ...): maki/mha.f:83-96, maki/attn-eq.f:71-72, maki/spec-attention-test.f:44,63, maki/equation-op-test.f:41, maki/prec-grammar-test.f:29, maki/adam-attn-grad-test.f:179 - files whose COMMENTS already carry Unicode (·, ᵀ), so the byte-oriented exemption does not apply. Respell them all to the canonical form. maki/spec-test.f is the spelling test matrix itself (proves both members of each confusable pair parse identically) - its deliberate ASCII cases STAY. Verify first that the prefix form accepts a MULTI-INDEX contraction list (Σdad dac - spec.f:338 region; attn-eq.f's equations contract two indices); if the lexer's prefix list handling cannot express it, THAT is a grammar gap to report (and fix in maki/spec.f with red-first negatives), not a reason to leave those two lines ASCII. Proof: respelling must be behavior-neutral - re-run every owning golden/suite and show byte-identical derived kernels/adjoints (the spec-test.f pair-identity theorem predicts this; prove it on the actual respelled lines). Also verify the MODEL: lines in the same files follow the current typed-signature authoring surface. Territory: the listed maki files (+ maki/spec.f only if the multi-index prefix gap is real).

Claim: agent=specpretty workspace=.jj-ws/fable-specpretty machine=spark (owns maki/mha.f attn-eq.f attn-eq-test.f spec-attention-test.f equation-op-test.f prec-grammar-test.f adam-attn-grad-test.f + maki/spec.f only if the multi-index prefix gap is real; disjoint from affine lane cad/registry/executor/backward footprint and adamw2 adam-train footprint)

---
title: Teach the pre-verifier plain create wrappers
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T11:02:08.040655+02:00"
---

Problem (r4-originmark, bd62bc07): `: MK ( -- ) create ;` then `MK :` loads (MK reads `:` as the new word's name) but check.f refuses it at the pre-verify step with E-UNDEFINED: src/habu/verify-source.f learns does> definers as name readers but not a colon definition whose body ends in a plain `create`, so it reads `MK :` as the start of a definition. Probe $HOME/.cache/tmp/kestrel-r4-originmark/probes/p1.f. Acceptance: the pre-verifier treats a word whose body creates (create, and the other definers it already knows, at the body's tail as the engine sees them) as a name reader the way it treats a does> definer, so p1.f checks rc 0 with its load output, and a wrapper that does not read a name is not mis-learned; a case through tools/check-test-lib.f seen to fail first; engine rebuilt (verify-source.f is baked), g1 == g2, two-gen. Files: src/habu/verify-source.f, tools/check-test-lib.f. Base: after 44279554 if both are in flight (same file).

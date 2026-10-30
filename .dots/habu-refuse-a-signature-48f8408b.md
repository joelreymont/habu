---
title: Refuse a signature nested past EFFECT-DEPTH-MAX by name
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T08:52:58.580786+02:00"
---

Problem: a signature's nesting past EFFECT-DEPTH-MAX (4096, named refusal MD-EFFECT-DEPTH) has no named refusal in the signature parser: 4200 nested binders refuse by name, 8300-45000 die with 'term walk too deep (cyclic term)' rc 76 (parent engines die the same on 8300 nested `ptr` with no binders), 65600 overflow the machine stack in the parser rc 102 (probes: tim-binddepth/.out/review/p/, run through CHECK-CANDIDATE! which takes raw text). Today the 8000-byte body cap keeps a `:` body under about 790 binders; once habu-raise-the-per-b34edffa lets the body grow, a source reaches the unnamed die. Found by the Opus review of tim's habu-grow-the-signature-acdc8235. Acceptance: the signature parser refuses nesting deeper than the existing EFFECT-DEPTH-MAX by name for every kind of nesting (binders, quotations, pointees), so bound-variable depth fields stay below BVAR-REGION-BIT $10000 and neither the false cyclic die nor the stack overflow can be reached; no new limit, the existing one applied at the parser; a test past the limit refuses by name through the real load path; the 4200 probe unchanged. Files: src/core/checker.f (the signature parser: SIG-FORALL-DOMAIN and the nesting words, EFFECT-DEPTH-MAX, MD-EFFECT-DEPTH). Verify: the new refusal test; the full suite (shared checker behaviour). Depends: habu-grow-the-signature-acdc8235. Ownership: the signature parser's depth. Claim: unassigned.

---
title: Replace the signature pool with a generated declarations file
status: open
priority: 2
issue-type: task
created-at: "2026-08-18T19:38:29.083141+02:00"
---

User challenge 2026-08-18: the 245KB signature text serves checked code naming BAKED words - but only PUBLIC words are nameable. Measured split: 1606 public / 5192 private / 0 global. Private words live in sealed packages; no checked program can open or compile into one (the seal, checked-habu threat model), so their rows serve nobody BY CONSTRUCTION - the producibility argument. Prune the capture to public rows only: expected 245KB -> ~60KB. CASCADE, handle honestly: (a) the capture-side audit currently requires EVERY checked window record to carry a row - its clause set gains 'private = exempt' with the seal as the argument, and the partition assertion (6798=94+checked) re-derives; (b) verify the intake never needs a private row (the chain's own internal calls resolve by the SEED's relocation, not the checker pool - confirm by measurement); (c) SIGSCOPE's bare-family fixture uses an EXPORTed public word - unaffected; (d) a mutation: a private row smuggled in reds by name (the pool carries only what is nameable). Rides the buffers-at-startup landing or follows it - same artifact surgery region.

SUPERSEDED BY USER RULING 2026-08-18 (the prune was the wrong
fix - the user's question "runtime dispatch uses xts, why carry
the pool?" is correct and terminal): the pool serves ONLY the
compilation of new checked code against the baked API - that is
a HEADER FILE's job, not binary payload. NEW SHAPE: at build
time, GENERATE a declarations file (chain-decl.f or kin) from
the checker's certified store - the ~1606 public signatures,
one row each, through the existing render path (2003/2003
round-trip proven). A program calling the compiler API requires
it like a C header; the build payload adds one require. THE
BINARY CARRIES ZERO SIGNATURE BYTES. DELETES: the baked pool +
its artifact sections (VERSION bump), the lazy intake at
DO-TOK's miss leg, the CHECK-RETRY loop, the AOT-SIG cells -
the subsystem was correct engineering against an invented
requirement (self-describing binary). KEEPS: the capture at the
three producers and the certified store (they GENERATE the
file); the window audit (every public checked word has a row IN
THE FILE); SIGSCOPE's scope discipline applies to the file's
rows (qualified spelling in the generated text makes it moot -
generate QUALIFIED, the TDGEN precedent). Probe first: TRUST-
row semantics for package-qualified names in a consumer (the
one-row experiment that started all this used a global); if
qualified TRUST rows need machinery, checkpoint. The registry
(46KB types) question rides along: families the API's
signatures NAME must exist in the consumer - measure whether
public signatures reference in-window families (the census
knows) and whether the decl file must carry NEWTYPE rows too.
Acceptance: the T2-class reproducers green VIA THE FILE in both
engines; the binary's signature bytes ZERO; the subsystem
deletion attributed; a consumer without the require gets the
honest E-UNDEFINED.


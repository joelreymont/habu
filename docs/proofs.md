# Proof Integrity

Rules for published results, models, manifests, and parity gates.

- A published claim must constrain the implementation, rather than merely
  restate its model. Use a counterexample or targeted mutation when needed to
  establish that a check detects the defect it claims to catch.
- A result that restates the model's own definition (proving `push` appends
  when `push` is defined as appending) constrains nothing. Demote to `Lemma`
  or delete; never publish it in a manifest, which inflates apparent coverage.
- A family of concrete examples failing under one mutation is one result under
  many names. Generalise to a universal statement or keep a representative few.
- State a model's assumptions, unsupported cases and known counterexamples.
- An unproved claim remains unproved. Record the limitation where the claim is
  documented; never weaken it silently until it passes.
- Tests, proofs and measurements establish different properties. Describe what
  was actually checked without mandatory mutation campaigns or extra ledgers.

`formal/Common/Storage.v` is an abstract, finite storage model. Its fixed scratch
mapping and older registry/exception descriptions do not model the runtime's
dynamic chunks, direct slot lookup, OS resource release or `finally` cleanup.
The storage proof entry checks that model's statements and assumptions and
compares selected operation examples with the runtime; it does not establish
parameter parity or implementation refinement. Dynamic growth, normal and
exceptional lifetime, owner/stale rejection and state after refusals are tested
through the native paths in `test/compiler/ir-context.f`, `ir-arena.f` and
`ir-storage-manifest.f`.

`formal/Common/Reloc.v` proves round trips for recorded sites in its model.
The relocation tests compare selected vectors with the shipped instruction
sequences using a mnemonic interpreter; they do not prove that every address
producer records its sites, nor execute the address-cell writer in that model
comparison. Actual writer and restored-image behavior is tested separately in
`test/snapshot-writer.f` and `test/app-image.f`.

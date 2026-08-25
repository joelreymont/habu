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

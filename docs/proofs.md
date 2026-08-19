# Proof Integrity

Rules for published results, models, manifests, and parity gates.

- Worth test for a published result: could a plausible change to the Habu code
  falsify it? If not, it does no work. Mutate the CODE to decide — a result
  that only breaks when the model is edited with it has no independent content.
- A result that restates the model's own definition (proving `push` appends
  when `push` is defined as appending) constrains nothing. Demote to `Lemma`
  or delete; never publish it in a manifest, which inflates apparent coverage.
- A family of concrete examples failing under one mutation is one result under
  many names. Generalise to a universal statement or keep a representative few.
- Every model carries counterexamples and negative results (a guard's removal
  admits a bad state). A model with none is suspect.
- Unprovable is a RESULT: record it in MODEL GAPS. Never weaken a statement
  until it passes.
- Falsify every parity-gate clause by mutation before believing the gate.

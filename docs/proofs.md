# Verification claims

Rules for published results, reference models and tests.

- A published claim must constrain the implementation, rather than merely
  restate its reference model. Use a counterexample when needed to establish
  that a check detects the defect it claims to catch.
- A result that restates a model's own definition establishes no implementation
  behavior. Test the production path instead.
- State assumptions, unsupported cases and known counterexamples.
- An unproved claim remains unproved. Record the limitation where the claim is
  documented; never weaken it silently until it passes.
- Tests and measurements establish different properties. Describe what was
  actually checked without mandatory mutation campaigns or extra ledgers.

The theorem-prover subsystem has been removed. Native checks cover executable
behavior; they do not establish a formal soundness or refinement theorem.

Dynamic growth, normal and exceptional lifetime, owner/stale rejection and
state after refusals are tested through the native paths in
`test/compiler/ir-context.f`, `ir-arena.f` and `ir-storage-manifest.f`.

The relocation tests compare selected vectors with shipped instruction
sequences using a mnemonic interpreter. They do not establish that every
address producer records its sites. Actual writer and restored-image behavior
is tested separately in `test/snapshot-writer.f` and `test/app-image.f`.

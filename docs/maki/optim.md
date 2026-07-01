# Maki optimizers

`maki/optim.f`: per-element float update rules (the math an optimizer applies to each
weight given its gradient), pure checked Habu on the float stack. `maki/optim-tensor.f`
lifts those rules over contiguous tensor buffers; `TT-ADAM!` is the checked host
golden for the device lowering.

## Rules
- **SGD** `(w g lr -- w')` = `w − lr·g`.
- **SGD-MOM** `(w g v lr mu -- w' v')`: `v' = mu·v + g`, `w' = w − lr·v'`.
- **WEIGHT-DECAY (L2)** `(g w wd -- g')` = `g + wd·w` (apply before the update).
- **Adam** (factored): `ADAM-M (m g b1 -- m')` = `b1·m + (1−b1)·g`;
  `ADAM-V (v g b2 -- v')` = `b2·v + (1−b2)·g²`;
  `ADAM-W (w m' v' lr eps bc1 bc2 -- w')` = `w − lr·(m'/bc1)/(√(v'/bc2)+eps)` (host
  `fsqrt`); `ADAM (… -- w' m' v')` composes them. `bc1 = 1−b1^t`, `bc2 = 1−b2^t` are
  the bias-correction denominators the caller tracks per step `t`.

## Design intent
Each rule is a small, individually tested checked word (a `T{ … }T`-style scale-and-
round assertion per rule + a full-step check). Tensor Adam applies the same scalar
golden per element and updates the parameter, first-moment, and second-moment buffers
in place. The tensor apply is an elementwise pass, **memory-bound** (roofline) → it
fuses onto the producing kernel's epilogue rather than launching its own kernel
(`docs/kernel-principles.md`, `maki/fusion.f`).

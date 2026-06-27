# Maki autograd — VJP rules + numeric gradcheck

`maki/autograd.f`: tensor ops paired with their VJP (backward) rules, at the element
level, each **numerically verified** against a central finite difference — the
gradcheck the type system cannot give (the strongest form of the verified-gradient
thesis, demonstrated numerically, not just type-checked).

## Forward + backward pairs (the VJP table)
- `ADD-F (x y -- z)` / `ADD-BWD (dz -- dx dy)` = `dz, dz`.
- `MUL-F (x y -- z)` / `MUL-BWD (dz x y -- dx dy)` = `dz·y, dz·x`.
- `RELU-F (x -- z)` = `max(x,0)` / `RELU-BWD (dz x -- dx)` = `x<0 ? 0 : dz`.

Each op's backward is the transpose of its linearization (VJP). The table is the
maki-level orchestration that, on device, lowers onto the Habu-PTX primitive VJP table
(the reverse-mode AD-as-syntactic-reversal in `lib/ptx/ad-dag.f`; see `docs/autograd.md`).

## Gradcheck
For each op, the analytic VJP is compared to `(f(x+ε) − f(x−ε)) / 2ε`. A mismatch fails
the test. This is what makes "the gradient is correct" a *checked* claim at the maki
layer, complementing the device finite-difference gradcheck of the auto-derived
`SOFTMAX-ROWS-BWD` on the Orin.

## Design intent
Define-forward → checked-backward: a user writes the forward op; its VJP is registered
+ gradcheck-verified. Composed forward graphs reverse by composing the VJPs (the
backward pass is "just more matmuls and reductions" — `docs/kernel-principles.md`).

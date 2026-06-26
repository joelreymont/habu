# Maki — an ML framework on Habu + Habu-PTX

Maki is the ML *framework* layer: tensor/array types, autograd orchestration,
optimizers, ONNX import, the training/eval loop, and the LLM-target eval harness.
It is built **on** Habu and its checked PTX kernel backend. See the root
[`PLAN.md`](../PLAN.md) for the full design and the dot chain.

## Boundary (BLOCKING)

- **One-way dependency: `maki → habu`, never the reverse.** Maki loads Habu
  libraries (`lib/*.f`, the PTX vocabulary `lib/ptx*.f`); Habu core, the gate, and
  the fixpoint must never reference `maki/`. (A dependency lint is owed —
  dot `habu-add-maki-one`; until it lands, the seam is held by review + the gate's
  explicit allow-lists.)
- **Fenced out of the trust root.** `maki/` is **not** in `TRUSTED.md`, **not** in
  the byte-for-byte fixpoint, and **not** a native-gate dependency. It is
  application Forth run by `bin/hb`, naturally outside the self-hosting fixpoint.
- **Still strictly checked/typed Habu.** The fence excludes maki from the *trust
  manifest*, not from the *checker*. Maki definitions use real typed effects and
  are verified through maki's own `bin/hb --load` path.
- **Extractable.** Treat the habu↔maki seam as an API even in-repo; extract `maki/`
  to its own repo when the Habu-PTX API stabilizes.

## Load prelude

Maki source loads the Habu libraries it needs, then maki files, e.g.:

```
bin/hb --load lib/errors.f lib/string.f lib/test.f \
  lib/ptx.f lib/ptx-tile.f lib/ptx-collective.f \
  maki/tensor.f maki/tensor-test.f
```

## Status

See [`STATUS.md`](STATUS.md). Active work is in the root dot chain (`maki-*` /
`habu-maki-*` dots).

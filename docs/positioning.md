# Habu Positioning and Subtitles

Reference for README, docs, and announcement copy. The implementation plan this
language fronts is `docs/model-cad.md`.

## Selected positioning (2026-07-04)

> **Habu — Model CAD for GPUs**

> **Change the model; Habu re-fuses, re-tiles, and re-tunes the GPU path.**

Decisions and rationale:

1. **Slogan kept.** "Model CAD for GPUs" is an ownable category — nobody in ML
   tooling claims the CAD analogy — and the audience that matters (systems and
   performance people) gets it instantly. The "CAD model / 3D modeling"
   misread is real but defused by the subtitle and by placing the CAD-mapping
   table directly under the hero.
2. **Subtitle is the verb promise, not the noun list.** "One REPL for fusion,
   coalescing, tiling, validation, and tuning" names compartments; the verb
   line states what happens when the user acts, and the "re-" prefix carries
   the CAD essence — the design stays live. The noun list survives as the
   loop-contents line inside the hero body, not the headline.
3. **The agent claim is stated as mechanism, not slogan.** The only
   differentiation with measured evidence today (`docs/eval-triton.md`) is
   author-time rejection of the stack-discipline bug class with zero GPU —
   exactly what makes high-volume LLM-proposed kernels safe to accept. The
   hero states the gate protocol in one factual sentence.
4. **No aphorisms in the README.** Honest and technical throughout; every
   claim is a mechanism or a measurement. Taglines live only in this document,
   for external copy where a tagline is structurally required.
5. **The Moore lineage is explicit.** Chuck Moore is the inspiration for the
   project, and the README says so in a dedicated Lineage section: OKAD —
   his own full-custom VLSI CAD in a few thousand lines of Forth — produced
   fabricated silicon (ShBoom, MuP21, F21, GreenArrays GA144) when chip
   design was assumed to require commercial EDA seats. PyTorch and Triton are
   framed as this era's industrial suites; Habu as the personal CAD system.
   The honest divergence is stated rather than hidden: Moore trusted one
   careful human and had no use for type systems; Habu's highest-volume
   author is an LLM, so the discipline is enforced by a checker.
6. **Smallness evidence is the binary, not line counts.** `bin/hb` — the
   self-hosted engine that type-checks, JITs, and rebuilds itself to a
   fixpoint — is under 128 KB (115,831 bytes measured 2026-07-04). Line
   counts drift and invite quibbling; the binary is one checkable number.

## Recommended hero

```markdown
# Habu — Model CAD for GPUs

**Change the model; Habu re-fuses, re-tiles, and re-tunes the GPU path.**

Habu keeps the optimization loop at the model level: one live REPL where
fusion, memory coalescing, tiling, validation, profiling, and tuning stay in
sync with model edits — the way EDA keeps placement, routing, design-rule
checks, and timing in sync with a schematic.

Whether a change is typed by a person or proposed by an LLM, it passes the
same gates before promotion: author-time type check, golden test against a
reference, gradcheck for generated backward code, and device profile.
```

Why this works:

- It starts with the workflow benefit as an action, not a feature list.
- It avoids saying "Habu builds GPU code," which is table-stakes.
- It still names the hard optimizations in the body line.
- It states the agent claim as a verifiable protocol, not a slogan.
- It leaves checker/Forth/PTX details as implementation machinery.

## The Moore lineage

Rules for using the lineage in any copy:

- Name Chuck Moore, OKAD, and the GA144; the history is checkable and the
  strongest form of the claim.
- The parallel is structural, never nostalgic: one small live tool that does
  the entire job, owned end to end, understood whole.
- Frame PyTorch/Triton as industrial suites respectfully — capable, enormous,
  opaque end to end — never as strawmen.
- Always state the divergence: Moore rejected type systems; Habu checks
  because its highest-volume author is an LLM.
- The lineage is identity, not evidence. Performance and correctness claims
  cite `docs/eval-triton.md` and gate results, never the philosophy.

## Strong subtitle options

### Best all-around

> **One REPL for fusion, coalescing, tiling, validation, and tuning.**

### Shorter

> **Turn model blocks into fused, tiled GPU pipelines.**

### More technical

> **Automatic mega-fusion, coalesced memory, and hardware-aware tiling for GPU models.**

### REPL-centric

> **A live GPU optimization loop for model blocks.**

### Benefit-centric

> **Make model changes without redoing the kernel engineering.**

### Agent-centric

> **Let agents explore GPU schedules while Habu validates, measures, and caches the winners.**

### More memorable

> **Fuse the graph. Tile the hardware. Measure the result.**

### More ambitious

> **Whole-model GPU optimization, live.**

### More practical

> **From model edit to fused GPU pipeline in one interactive loop.**

### Strong for README first paragraph

> **Habu keeps performance work at the model level: it fuses operations, plans
> memory, chooses tiles, validates outputs, tunes schedules, and runs the result
> from one REPL.**

## Avoid these as primary subtitles

### "Describe the model. Habu builds the GPU implementation."

Too generic. PyTorch, JAX, XLA, TorchInductor, TVM, and other systems can
plausibly claim similar wording. It does not say what is hard or differentiated.

### "Checked Model CAD for GPUs"

Accurate internally, but weak externally. Users assume correctness should be
built in. "Checked" is a mechanism, not the benefit.

### "Check the math. Tune the kernels. Ship the cubin."

Too technical and puts work on the user. "Cubin" is implementation detail.

### "GPU CAD"

Good category shorthand, but ambiguous. It can sound like CAD for designing GPU
chips. "Model CAD for GPUs" is clearer.

## Messaging hierarchy

### Lead with the pain

```text
Stop rewriting models as kernels.
Make model changes without redoing the kernel engineering.
Keep the optimization loop at the model level.
```

### Then state the mechanism

```text
Habu fuses operations, plans memory, chooses tiles, validates results, tunes
schedules, and caches the best GPU artifact.
```

### Then state the differentiator

```text
All of this happens from one REPL, with the model, fusion plan, memory plan,
schedule, validation result, and profile report visible in one place.
```

### Then mention internals only after the user benefit

```text
Under the hood, Habu uses a small Forth-like language, typed GPU words, PTX
generation, and structural checks to make this automation safe and
agent-friendly.
```

## Positioning against existing tools

### Against PyTorch-style frameworks

Do not say "PyTorch cannot run models on GPUs." It can.

Say:

> PyTorch is excellent for expressing models. Habu is about keeping the
> optimization loop at the model level when performance requires fusion, tiling,
> coalescing, generated backward code, and shape/device-specific tuning.

### Against Triton

Do not say "Triton cannot be fast." It can.

Say:

> Triton gives you a productive way to write a kernel. Habu tries to avoid
> making the user decide every kernel boundary by hand. The model block is the
> design artifact; fused kernels and schedules are derived, validated, tuned,
> and cached.

The earned comparison today lives in `docs/eval-triton.md`: Habu-PTX moves the
stack-discipline error class to author time and reaches SAXPY v4 bandwidth
parity on the Orin. Do not claim more than that until measured.

### Against black-box compilers

Do not say "compilers cannot optimize." They can.

Say:

> Habu should be inspectable and interactive. Fusion plans, memory plans,
> schedules, validation, and profile rows are first-class outputs, not hidden
> compiler side effects.

## Tagline shortlist

1. **Stop rewriting models as kernels.**
2. **Whole-model GPU optimization, live.**
3. **Turn model blocks into fused, tiled GPU pipelines.**
4. **Fuse the graph. Tile the hardware. Measure the result.**
5. **Make model changes without redoing the kernel engineering.**
6. **A live GPU optimization loop for model blocks.**
7. **Automatic mega-fusion and hardware-aware tiling for GPU models.**
8. **From model edit to fused GPU pipeline in one REPL.**
9. **Keep the model. Automate the kernel work.**
10. **Model-level intent, hardware-level performance.**

Short social tagline:

> **Stop babysitting kernels.**

These taglines are for external copy only (social posts, talk titles). The
README carries no aphorisms; see the decision log at the top of this document
for the selected pair and the rules.

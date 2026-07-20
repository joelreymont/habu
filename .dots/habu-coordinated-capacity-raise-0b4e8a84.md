---
title: Coordinated capacity raise for 12-block GPT-2
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-20T15:47:33.889332+02:00\""
---

The measured remaining walls between the landed 2-block differentiable stack (de52c0b8) and nanoGPT's 12-block target, from the MIR-cap lane's consumer audit - ONE coordinated raise, each with the fixed-cap+named-die idiom, red-first cap+1 regressions, and measured DATA justification: (1) MSRC-CAP 2048 (maki/cad.f capture-source buffer) - the BINDING wall TODAY: a 3-block MODEL: body already dies E-CAD-SYNTAX during capture-DSL body parse (proven by the MIR lane; the 47-input signature parses, the block-C body capture does not); (2) BW-NCAP 128 (maki/backward.f:71) - bounded by FORWARD node count, binds at ~9 blocks (12-block fwd ~ 74+14x11 forward-only extrapolation; measure honestly); (3) EX-ARENA-CELLS $8000 float cells (executor) - 2-block uses ~5200, 12-block ~36000 overflows; (4) the PTX-lowering caps in lower/red.f lower/mm.f lower/ew.f lower/launch.f fusion-plan.f (enumerated by the audit, unexercised at 2 blocks - measure at the target). Sequence by binding order: MSRC-CAP first (it blocks even a 3-block FORWARD), then BW-NCAP/EX-ARENA at their measured bind points. Acceptance: a 12-block differentiable GPT-2-small-shaped model builds (train-lock optional if slice budget forbids - a build+gradcheck-sample lock suffices, justify), node/arena/source accounting pinned at 12 blocks, all caps carry regressions. Territory: maki/cad.f model-ir/backward/executor lowering caps + capacity regressions.

Claim RELEASED 2026-07-20 (agent=caps12 stopped by the orchestrator mid-flight, no landing): Joel ordered the PROPER fix instead of the interim constant raise - see habu-size-model-proportional-16994186 (derive-from-model sizing), which supersedes this dot. Partial finding worth keeping from the stopped lane: a 12-block MODEL: capture dies -5045 (not the -5055 E-MIR-CAP class) - identify that code during the derive work; the 3-block E-CAD-SYNTAX attribution to MSRC-CAP remains UNVERIFIED.

2026-07-20 SUPERSEDED by habu-size-model-proportional-16994186 (Joel: build the derive-from-model design instead of raising constants). This dot's measured numbers (88 B/node, 723-node 12-block extrapolation, arena ~36000 cells at 12 blocks, BW-NCAP binds ~9 blocks) remain the sizing evidence for the derive lane. Keep open only as the numbers record until the derive landing closes it.

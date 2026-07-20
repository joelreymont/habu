---
title: Fuse multi-head QKV projection
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T23:34:28.076569+02:00"
---

maki/mha.f defines MHA-QPROJ, MHA-KPROJ and MHA-VPROJ as three separate SPEC contractions and invokes all three for every head. This rereads the same X and dispatches 3*H matrix products before attention; because the fixed oracle orchestrates generated words directly outside planner IR, no later fusion pass can combine them. GPT-2 conventionally applies one X by Wqkv projection (including one combined bias) and views the result as Q/K/V. Measure host contraction calls, input bytes, JIT/generated code, and target kernel launches for representative GPT-2 shapes. Replace the three projections with one checked fused QKV contraction plus typed views when measurement confirms the expected traffic/launch reduction; derive all forward and adjoint slices from one layout contract. Preserve exact Q/K/V numerics and gradients. Prove external forward/backward parity, each head/layout boundary, bias, B/T/C/H/hd variants, no materialized copy beyond the fused output, one projection dispatch, device SASS/traffic/timing improvement, and source/JIT/DATA/CODELEN reduction without regressing small shapes. Files: Maki MHA/SPEC/model/lowering tests. Serialize with habu-complete-trainable-multi-39e26b3d; this dot owns projection fusion only.

2026-07-20 SERIALIZED behind habu-complete-trainable-multi-39e26b3d (spark lane running): fusion measures against the trainable baseline that lane lands.

2026-07-20 serialization released (trainable MHA landed 2efa4388 - the baseline to measure against exists).
Claim: agent=qkvfuse workspace=.jj-ws/fable-qkvfuse machine=spark (owns maki/mha.f + mha tests; measurement-first per the dot - GPU for launch/traffic counts where the harness supports)

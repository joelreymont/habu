---
title: Compose tied wte/LM-head into the block
status: closed
priority: 1
issue-type: task
created-at: "2026-07-20T22:41:31.061036+02:00"
closed-at: "2026-07-20T23:29:11.929093+02:00"
close-reason: "Landed 6ea65203: wte and the LM head are one tied parameter inside the real GPT-2 block composition (gptblock-attn-test.f +154/-10), consuming the recovered 482af1a6 machinery. Orientation stated in-file: wte (V,C) is the single stored parameter, the head weight is its transpose mirror refreshed after every step and asserted bit-identical, tied grad = Gwte + Gwlm^T. Proof: write-through-one-role visible in the other; both gradient contributions proven present (tied grad differs from embed-only AND head-only, red-first against the untied base which drops head dist2~0.805); tied-parameter central-FD gradcheck incl. a never-gathered row where the head path is the whole signal, teeth case rejects; 12-step Adam drives mean CE milli 1665->27 run-twice bit-identical; tie asserted held after every step. Full cold gate green at the merged tip (the lane's local DOT-DEP-DUPLICATE red was its pre-fix base, already fixed on master by e5232251). Remaining composition gaps stay owned by the sibling sub-dots of habu-gpt-2-composition-a90e901e"
---

Integrate the shared wte/LM-head parameter with summed gradients INSIDE the block composition (gptblock-attn-test.f binds wte slot 0 and wlm slot 12 as independent buffers, separately Adam-updated at :172/:178) - not a standalone trainer. The tying mechanism with summed grads landed 482af1a6 (recovered orphan); consume it. Prove: one storage, both gradient contributions accumulated, gradcheck on the tied parameter, training still reduces loss, run-twice locked.

Claim: agent=tiewte workspace=.jj-ws/fable-tiewte machine=spark (owns the tie-in-block change to maki/examples/nanogpt/gptblock-attn-test.f + consumed tie machinery; NOTE the external-golden lane concurrently edits the same test file - orchestrator hand-merges)

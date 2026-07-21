---
title: Repair gforth install broken by scratchpad wipe
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-21T06:39:53.655558+02:00\""
---

Machine-provisioning defect found by the ptxcuda lane (2026-07-21): ~/.local/bin/gforth on spark hardcodes a dist path inside a PRIOR SESSION'S scratchpad directory, which the reboot deleted - so the no-binary recovery path (tools/bootstrap.sh, needed twice this week) is broken whenever bin/hb is lost. Reinstall gforth 0.7.9 snapshot PROPERLY under ~/.local (real prefix, no scratchpad paths; 0.7.3 is banned per policy), verify tools/bootstrap.sh end-to-end once (HABU_ALLOW_BOOTSTRAP=1, byte-fixpoint reached), and record the install recipe in the dgx-spark provisioning docs so it survives re-provisioning.

Claim: agent=gforth workspace=.jj-ws/fable-gforth machine=spark (owns the user-local gforth 0.7.9 reinstall + bootstrap.sh end-to-end verification IN ITS OWN WORKSPACE + the dgx-spark provisioning doc recipe)

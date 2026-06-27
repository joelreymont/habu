---
title: Commit checked-Habu model-driven grader (eval-author.f)
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T15:12:57.986685+02:00"
---

The model-driven pass@k/repair experiment (docs/eval-triton.md) graded candidates with throwaway /tmp scripts (grade_habu.sh, grade_habu_sm.sh, grade_one.py), so it is NOT reproducible from the tree. Build a checked Habu tool maki/eval-author.f: GRADE-AUTHOR ( a u task -- verdict ) reads a candidate kernel source, runs checker->emit->ptxas->device-golden reusing the eval-device.f (SAXPY) + eval-device-sm.f (softmax) pipelines, returns 0=author-reject / 1=device-wrong / 2=green AND surfaces the located checker diagnostic for the repair loop. Add maki/eval-author-test.f over known correct+buggy SAXPY/softmax candidates -> expected verdicts; add to the maki gate (maki/README.md). The Triton baseline + the subagent generation arm stay external+documented (Habu Only). VERIFY: maki gate green; correct->2, x+y->1, no-store->0, softmax B- not B/ ->1. Deps: none (reuses eval-device*.f).

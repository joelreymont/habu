---
title: "Maki: cad.f missing transitive lib/float require"
status: open
priority: 2
issue-type: task
created-at: "2026-07-05T00:43:20.138396+02:00"
---

Ablation-lane finding: MODEL: capture sets the float default tolerance via POW10 but maki/cad.f does not require lib/float.f transitively - masked in the gate by load order; a standalone consumer needed an explicit require lib/float.f. Fix: add the require to maki/cad.f (or wherever POW10 is consumed), remove workaround requires in consumers, prove by loading a minimal cad.f consumer standalone.

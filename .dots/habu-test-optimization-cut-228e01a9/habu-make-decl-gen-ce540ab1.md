---
title: Make decl-gen-probe load and run
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T09:54:10.360433+02:00"
---

Problem (r4-dotinline lane, fcf2e4f3b, dot 70a8af22): tools/decl-gen-probe.f cannot run as its header documents. On the release engine `bin/hb --load tools/decl-gen-probe.f` dies E-UNDEFINED: TFAM-ACTIVE-PKG$ (rc 70; the word exists only in the whitebox engine). On a whitebox engine it dies after its report output: `hb: ;using would close a using opened outside the package` at :157, because its two `using` lines (:47-48) precede `package DECL-GEN-PROBE` (:50) while the two `;using` (:156-157) follow `;package` (:152). Nothing runs it, so it rotted unseen. Acceptance: either the probe loads and runs its documented usage (header names the engine it needs) to rc 0 on a real subject, with a row that runs it so it cannot rot again; or, if no current work needs it, delete it and every reference, saying why. Write the failing run first. Base: after the master merge (it reads sumtype.f/type-family.f words master changed).

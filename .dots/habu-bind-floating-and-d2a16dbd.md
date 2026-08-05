---
title: Bind floating and mixed-class argument registers
status: open
priority: 2
issue-type: task
created-at: "2026-07-31T20:34:49.669240+02:00"
---

src/compiler/a64-effect.f now carries the general-register calling convention as two ordered lists (gpr-arg, gpr-out) and src/compiler/native/regalloc.f pre-colours from them. The floating side is still a pair of sets, because the A64IR dialect has no floating value class: A64IR:GPR-TYPE and MEM-TYPE are the only two, so a floating argument has no value to pre-colour. Two things arrive together when the floating register class lands in the dialect: an ordered fpr-arg/fpr-out pair beside the general ones, and a position model that says which CLASS each argument position takes - AAPCS assigns f(int,double,int) to x0, d0, x1, so the general list's position i is NOT the block argument's position i once classes interleave. Until then A64RA reads argument i of the block as argument i of the convention, which is exactly right for the integer-only subset and wrong the moment a float is passed. Owners: A64EFF, A64RA, A64RAV, A64IR.

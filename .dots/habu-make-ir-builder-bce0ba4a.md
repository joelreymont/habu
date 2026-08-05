---
title: Make IR builder creation transactional
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T10:36:19.495685+02:00"
---

CG-07. src/compiler/ir/build.f:605-638 allocates 17 component arenas before publishing builder ownership and has no unwind path; a fourth builder failing partway leaks 13 arena slots, after which even a one-arena allocation fails E-IR-ARENA-SLOTS (probe exit -6657 twice). Fix: publish-or-free — on any component allocation failure release every component already acquired in reverse order and rethrow the original error; publish the builder only after all components exist. Part of the unconditional-retirement class (see the checker-capability dot).

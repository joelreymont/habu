---
title: Retire native HIR observer
status: closed
priority: 1
issue-type: task
created-at: "2026-10-04T05:08:20.586680+03:00"
closed-at: "2026-10-14T23:02:39.650766+03:00"
close-reason: HIR observer retirement f32a3451 and legacy host-row fix 894b7a47 are ancestors of qualified ed285fdc. Independent Astra follow-up found no defects and verified unchanged registered coverage; ed285 native-build-entry and native-dictionary-publish both pass. Release-owner 663/663 native gate and byte convergence apply. Publication/invalidation callbacks remain; unrelated DATA/CODE rows still refuse.
---

HBR2 and UI-ADMIT were removed because they had no caller. Their target-neutral
post-freeze NBACK observer has no production caller and was removed from
the compiler, image layout and tests. Publication and invalidation callbacks
at $2CF8 and $2D00 remain in use.

The native build retires the former host's $2CF0 CODE declaration through
ADDRESS-CELLS before checking fixed registrations; DATA at that offset and
other unknown fixed rows still refuse. The registered native-build entry
exercises those host rows through the real build path. The registered native
dictionary publication suite retains the fatal post-commit callback case.
Native and generation checks and focused reviewer follow-up remain pending.

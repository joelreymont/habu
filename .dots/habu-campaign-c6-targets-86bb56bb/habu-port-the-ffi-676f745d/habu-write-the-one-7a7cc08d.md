---
title: Write the one kernel exit line on x86-64
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T15:34:38.834675+02:00"
---

Problem: arm64 kernel exits write one line, `<class> habu: <text>[ <16 hex>] lr <16 hex>`, before they exit (feature of habu-print-before-every-37687d3a); the x86-64 kernel still writes the old texts at src/habu/boot-x64.f:542-545, src/arch/x86-64/rt.f:166 and src/habu/kernel-x64.f:3899, :4098, :4530, :4903-4926. Six suites shared through test/gate-stdlib-cases.f expect the arm64 line on every host: engine-stack-machine, engine-stack-lifecycle, compiler-native-eval, ffi-callback, friend-arena-seal, native-gate-dictionary. Acceptance: the x86-64 kernel writes the same line at every site; those six suites pass on an x86-64 engine. Depends: none. Claim: unassigned.

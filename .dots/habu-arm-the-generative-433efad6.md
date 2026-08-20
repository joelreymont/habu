---
title: Arm the generative definers before the decl machinery
status: open
priority: 2
issue-type: task
created-at: "2026-08-20T10:56:54.778632+02:00"
---

From storage-1 (2026-08-20): TYPED-VARIABLE is generative - it evaluates its accessors through TDECL-EVAL-XT, armed at src/core/include.f:518, prefix row 34. The decl machinery (generated-declaration.f 20, structure-decl.f 23, enum-decl.f 24 storage sites) sits below that row, so the capability master 0cc8d823 landed is unreachable there: conversion dies 7121 with the type admissible. Distinct from route 3 (64078d43, the recording gap) but adjacent: moving INCLUDE-EVALUATE's arming above prefix row 20 unlocks the eight convertible sites. Also record: src/habu/hide.f's 5-6 sites are stage2 sources compiled by the OLD engine (build-fixpoint.f:988) - they convert one full release after any capability lands; schedule it rather than discover it. snap-lib.f 5 sites unprobed (snapshot writer, convert deliberately).

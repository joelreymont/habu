---
title: A character literal class for the checker's reader
status: open
priority: 2
issue-type: task
created-at: "2026-08-12T14:00:00.000000+02:00"
---

Scoped out of the literal-authority fix (79c570ed checkpoint, 2026-08-12): the engine's number reader has no character spelling (C-NUM-SIGN/BASE/DIGIT accept sign, dollar, digits, one dot), and char X / [char] X is a keyword plus a payload token the checker's reader deliberately SKIPS - feed.f's header lines 44-46 records it as open work and CHECKER-TAPE has four classes with no char class. Delivering it: a new reader event/class in src/core/checker.f plus a new tape kind in the chain, with the value asked of the engine surface that owns char (find it - the num-parse precedent says expose, never mirror). CENSUS 2026-08-12: [char] is used by 9 tree definitions (3 first-refusals measured, owners ca9e5541+0750ac90 for the compile-time forms) - cut-blocking per the user's no-refusals ruling. Files: src/core/checker.f, src/compiler/native/feed.f. Depends: habu-record-the-engine-79c570ed (landed d994661f).

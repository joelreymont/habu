---
title: "Read the compiled definition's own effect record"
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T09:06:00.359253+02:00"
---

Problem: at tier 1 with '0 set-check', src/compiler/native/compiler.f KEEP-ARITY looks the definition's effect record up by bare name, so a private or package definition whose body the checker rejects borrows the record of a same-named global: s1 ('1 set-tier 0 set-check', global ': P1 ( n -- n ) 1 + ;', then in package PK ': P1 ( n -- bool ) 1 + ;' and '5 P1 . cr') compiles and runs (prints 6, rc 0) on engine rb3/g1, while the same package body with no global (s1c) is refused E-NCOMP-ARITY -8579 rc 67 and with a global of another arity (s1b: global ': P1 ( -- n ) 1 ;', package ': P1 ( n -- n bool ) dup 1 + ;') is refused -8304 rc 67. Whether a body compiles depends on an unrelated global. Probes $HOME/.cache/tmp/kestrel-r4-keeparity/r/s1.f (r4-keeparity lane, dot dd06655c) and the lead's s1b/s1c (copied to $HOME/.cache/tmp/kestrel-r4-keeparity/r/). Acceptance: every effect-record lookup in the native compiler's RECORD/KEEP-ARITY path reads the record of the definition being compiled (its own package and wordlist), never a same-named word's; s1 behaves as s1c (refused with keeparity's named reason); s1b, s1c and a shadowing definition the checker certifies keep their behaviour; regression under test/compiler/ seen failing first. Base: after dd06655c lands. Baked: rebuild, g1 == g2 with .names, two-generation build.

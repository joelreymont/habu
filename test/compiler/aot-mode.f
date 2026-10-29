\ Select the optimizing compiler from OUTSIDE the file under test.
\
\ A suite whose assertions are tier-1 facts says `1 set-tier` itself, after its
\ harness requires. This file is for a caller running one unchanged file at both
\ tiers: the `*-aot` twin rows in test/gate-stdlib-cases.f, and parents spawning
\ `--load test/compiler/aot-mode.f <subject>`.

\ Only code compiled after this file takes the tier, so a twin row lists the
\ subject's harness (src/lib/test.f) before it. This file loads nothing: some
\ callers pass it inside a native build window (test/loop-obligations.f,
\ test/field-proj-boundary.f), where every loaded file is compiled by the
\ window.

package NATIVE-AOT-MODE
1 set-tier
;package

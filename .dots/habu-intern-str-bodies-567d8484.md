---
title: Intern string bodies where the capture can see them
status: open
priority: 2
issue-type: task
created-at: "\"2026-08-10T18:38:00.858781+02:00\""
---

NSTR's arena is create/allot at load time - below every capture window's d0 by construction - so chain string bodies are invisible to AOT capture even with a perfect site record (aotsite lane 2026-08-11). Stage A (this dot): NSTR interns via here/allot at intern time so bodies land in the live window, content baked by the bake-the-window dot. string.f's 'DATA is the only home whose lifetime is the image's' sentence is true for snapshot and FALSE for the AOT seed - correct the header. Acceptance: an AOT-captured chain-compiled s-quote returns its bytes, not zeros. Files: src/compiler/native/string.f. Depends: habu-per-site-relocation-bb9b6d70, the bake dot.

NOT STARTED, BY RULING (aotsite lane 2026-08-11). The acceptance above cannot
observe anything yet: the native chain does not run inside an AOT capture
window, so NSTR:INTERN is never called during the metabuild. Measured three
ways - the stdin metabuild closure is exactly src/core/include.f,
src/habu/aot-capture.f and src/habu/stdin.f (tools/stdin-closure-lib.f);
`bin/hb --load tools/srclist.f -- stdin` lists no src/compiler file at all; and
NSTR's only consumers are src/compiler/native/elaborate.f and
test/compiler/native-string.f, while the capture window is the REPL sources,
which require nothing. Landing Stage A now would put the right mechanism in
front of an acceptance nobody can run, which is how the 21-of-142 defect this
epic exists to fix went unnoticed for a year.

BLOCKED ON TWO THINGS, both of which must land first.
(1) habu-seed-the-chain-e98b03d4 (Stage B): the chain has to be in the window
for the acceptance to have a subject. Its naive one-require-row design is
already refuted by measurement on that leaf.
(2) habu-aot-has-no-0b01043c: NOT a follow-up to this, a PRECONDITION.
Interning is content-keyed, so equal bodies share one address. The moment the
chain is in the metabuild host, every literal it interned while the prefix
loaded sits below d0, and a window word that shares those bytes is handed that
pre-window address - which the capture now refuses by name and which kills the
build. here/allot fixes only the bodies interned AFTER the window opens, so it
is necessary and NOT sufficient.

DESIGN NOTE FOR WHOEVER PICKS THIS UP. The direction is right: a body's home
should be the live window it was compiled into, so intern via here/allot at
intern time rather than from the load-time `create ARENA ARENA-CAP allot`. Two
consequences to plan for - rows then hold absolute addresses rather than arena
offsets (the package already carries the PTR>N boundary and will need its
inverse), and the arena's own E-NSTR-CAP ceiling gives way to the DP heap's
bound, which has its own named refusal. string.f's header sentence "DATA is the
only home whose lifetime is the image's" is TRUE for a snapshot restore and
FALSE for the AOT seed, and correcting it belongs in the same change.


SUFFICIENCY ESTABLISHED (2026-08-12, prewindow lane): here/allot at intern
time is necessary AND SUFFICIENT for NSTR, PROVIDED the chain is compiled
entirely inside the window rather than split across d0 - the content-key
sharing fear on this leaf materialises only if the chain loads on BOTH
sides of d0. Keep it on one side and this dot's design stands as written.

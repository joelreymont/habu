---
title: Move whole values through >r at tier 1
status: open
priority: 2
issue-type: task
created-at: "2026-10-10T03:30:36.633380+03:00"
---

Problem: ~/.cache/tmp/carl-gfrest/c/c3-bundle-rstack.f (`: F ( trip n -- trip n ) 2>r 2r> ;` over a three-field STRUCTURE) and c3-record-rstack.f (`: G ( trip -- trip ) >r r> ;`) run at tier 0 (4 3 2 1 and 3 2 1, rc 0); tier 1 refuses both, `ncomp: cannot compile F` / `G`, -8519 E-NELAB-BUNDLE, rc 67, under the Habu loop and the engine loop alike (master 9751f482). Native counts cells, not values: src/compiler/native/hir-word.f:619-623 gives each return-stack word a fixed 1 or 2 cells (RSTACK-CELLS), elaborate.f:364-370 RSTACK-STEP hands that count to TO-R :338-345, and RSTACK-CK :334-336 refuses a base inside a glued value (VGLUE-ABOVE?). The language moves values: src/core/checker.f:3731-3773 (RS->R, RS2->R, RSR>, RS2R>) push and pop whole row items, tools/check.f certifies both reproducers, `: G ( trip n -- ) 2>r 2r> drop drop ;` certifies and `: G ( trip -- n n n ) >r r> ;` is refused with "actual: trip" (docs/forth.md:551, a multi-cell record is one logical value). test/compiler/native-rename-rows.f:187 and :293-295 (C-TORB, `( option<n> -- option<n> ) >r r>`) assert the refusal, and lib/errors.f:928 describes it as one cell of a value parked on the return stack, which certified source never does.
Acceptance: at tier 1 `>r r> r@ 2>r 2r> 2r@` move or copy the whole values the checker moved, whatever their cell count; both reproducers print what tier 0 prints, rc 0; C-TORB compiles and runs; lib/errors.f:928 no longer names the return stack. The reproducers join test/compiler/native-rstack.f (tier 1, production return-stack operations).
Files: src/compiler/native/hir-word.f, src/compiler/native/elaborate.f, lib/errors.f, test/compiler/native-rename-rows.f, test/compiler/native-rstack.f.
Verify: rebuild bin/hb per docs/gate.md; each reproducer under `bin/hb --load test/outer-loop-on.f <file holding 1 set-tier> <case>`; `bin/hb --load test/compiler/native-rstack.f` and `bin/hb --load test/compiler/native-rename-rows.f`; `bin/hb --load test/run.f`; two-generation build converges.
Host cases: the Gforth host's r30 r34 r35 (~/.cache/tmp/carl-gfrest/c/waiting/) join test/gforth/cases/ and match native.
Depends: none.
Worker: worker.

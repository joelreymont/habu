---
title: Compile an unconsumed quotation at tier 1
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T20:58:01.329971+03:00"
---

Problem: tier 1 refuses a quotation nothing consumes; the empty quotation first reported here is one case of it. ~/.cache/tmp/carl-gfrest/c/c11-quot-drop-checked.f (`: F ( -- ) [: 1 ;] drop ;`), c11-quot-drop.f and c11-empty-drop.f (`[: ;] drop`) compile and run at tier 0, rc 0; tier 1 refuses each, `ncomp: cannot compile F at [:`, -8651, under the Habu loop and the engine loop alike (master 9751f482), while c11-empty-execute.f (`[: ;] execute`) compiles at tier 1. Native: QCONSUMED-CK, src/compiler/native/elaborate.f:1897-1898, called from QBUILD :4218, refuses a quotation whose value no word consumes. The language admits it: tools/check.f certifies c11-quot-drop-checked.f. First found while landing habu-load-authored-src-4ef714a3 (`: W ( n -- n ) [: ;] drop ;`, sealport/logs/p2inv-master.txt, w-quot and w-quot-scalar). A quotation an enclosing quotation returns is refused the same way, since only the definition's own declared output fills a returned literal's convention (QRET1-FILL :1874-1887 reads `0 QSPELL`) and a quotation body's QEMIT-RETURN (:4198-4210) drops its values without filling one. ~/.cache/tmp/carl-gfrest/c/g3-q16.f (`trusted: F ( -- ) [: [: 1 ;] ;] execute execute . ;`), g3-q16-checked.f (its `:` twin) and g3-q16-ret.f (`: F ( -- [ -- [ -- n ] ] ) [: [: 1 ;] ;] ;`) print `1 2` rc 0 at tier 0 and tools/check.f certifies each; tier 1 refuses each `ncomp: cannot compile F at [:`, -8651, rc 67, under both loops. g3-q16-inner-consumed.f (`[: [: 1 ;] execute ;] execute`) compiles at tier 1. docs/forth.md:1020-1023: quotations nest, each `[:` with its own inputs, calls and `exit`.
Acceptance: at tier 1 a quotation dropped, stored or left unconsumed compiles as tier 0 compiles it: c11-quot-drop-checked.f, c11-quot-drop.f and c11-empty-drop.f print what tier 0 prints, rc 0; c11-empty-execute.f is unchanged. g3-q16.f, g3-q16-checked.f and g3-q16-ret.f print `1 2`, rc 0, at tier 1; g3-q16-inner-consumed.f is unchanged. The reproducers join the native suite's tier-1 quotation cases (the suite file a search for QBUILD's cases finds). test/tier.f TEST-REFUSAL-TOKEN (once habu-judge-trusted-shape-ce04bc77 lands) reaches an elaborator refusal that names a token through this departure (TN-ELAB, `[: 1 ;] drop`): move it to another such refusal of a program the language admits, or delete it if none remains.
Files: src/compiler/native/elaborate.f, the tier-1 quotation suite file.
Verify: rebuild bin/hb per docs/gate.md; each reproducer under `bin/hb --load test/outer-loop-on.f <file holding 1 set-tier> <case>`; that suite file; `bin/hb --load test/run.f`.
Depends: none.
Worker: worker.
Superseded: the one-pass codegen (docs/architecture.md, "The codegen is one pass over the checked events") deletes the tier-1 code this fixes; its reproducers become that codegen's cases. Do not start.

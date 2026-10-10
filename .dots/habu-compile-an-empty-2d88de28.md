---
title: Compile an unconsumed quotation at tier 1
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T20:58:01.329971+03:00"
---

Problem: tier 1 refuses a quotation nothing consumes; the empty quotation first reported here is one case of it. ~/.cache/tmp/carl-gfrest/c/c11-quot-drop-checked.f (`: F ( -- ) [: 1 ;] drop ;`), c11-quot-drop.f and c11-empty-drop.f (`[: ;] drop`) compile and run at tier 0, rc 0; tier 1 refuses each, `ncomp: cannot compile F at [:`, -8651, under the Habu loop and the engine loop alike (master 9751f482), while c11-empty-execute.f (`[: ;] execute`) compiles at tier 1. Native: QCONSUMED-CK, src/compiler/native/elaborate.f:1897-1898, called from QBUILD :4218, refuses a quotation whose value no word consumes. The language admits it: tools/check.f certifies c11-quot-drop-checked.f. First found while landing habu-load-authored-src-4ef714a3 (`: W ( n -- n ) [: ;] drop ;`, sealport/logs/p2inv-master.txt, w-quot and w-quot-scalar).
Acceptance: at tier 1 a quotation dropped, stored or left unconsumed compiles as tier 0 compiles it: c11-quot-drop-checked.f, c11-quot-drop.f and c11-empty-drop.f print what tier 0 prints, rc 0; c11-empty-execute.f is unchanged. The reproducers join the native suite's tier-1 quotation cases (the suite file a search for QBUILD's cases finds).
Files: src/compiler/native/elaborate.f, the tier-1 quotation suite file.
Verify: rebuild bin/hb per docs/gate.md; each reproducer under `bin/hb --load test/outer-loop-on.f <file holding 1 set-tier> <case>`; that suite file; `bin/hb --load test/run.f`.
Depends: none.
Worker: worker.

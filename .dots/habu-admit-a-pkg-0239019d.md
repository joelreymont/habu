---
title: Admit a package definition a using shadows
status: open
priority: 2
issue-type: task
created-at: "2026-10-10T05:34:48.938454+03:00"
---

Problem: at tier 1, under the Habu loop, a definition in a package whose bare name is both a global and a used package's public is refused `E-USING-SHADOW-GLOBAL`, uncaught throw 7141 (~/.cache/tmp/carl-gfrest/c/v1.f: global `MK`, `P:MK`, then `package Q ... using P ... : MK ( n -- ) drop ;`; kp1.f the same with a lying hook; kd.f a definer `MK` there whose clause is `@`, which tier 0 and the Gforth host run, printing 5). Tier 0 and tools/check.f admit both. docs/forth.md:364-365 makes shadowing a global from inside a package legal and :474-475 places E-USING-SHADOW-GLOBAL at a reference site; the name being defined is not a reference. Native: src/compiler/native/compiler.f KEEP-PRIOR (:210) asks the checker about the pending name through KEEP-ARITY's bare-name lookup (forth.md:375). The Gforth host's forget probes pk1 to pk5 and thr1 (~/.cache/tmp/carl-gfrest/c/forget/) reach it (pk3 and pk4: native renders E-USING-AMBIGUOUS for the defined W, which the program's hook then rejects; tier 0 refuses only T's bare W, 7144); test/compiler/native-hookless-reject.f ZERO-USED pins it at tier 1 (REFUSED-W).
Acceptance: at tier 1 a definition's own name is admitted as at tier 0, while a bare reference to an ambiguous tail in a body still refuses E-USING-SHADOW-GLOBAL and the forwarder arity rule (E-SHADOWED-ARITY) still holds where the tail is not ambiguous. v1, kp1 and kd join test/outer-interpret.f, ZERO-USED's tier-1 expectation follows, and pk1 to pk5 and thr1 match on the Gforth host, as do its cases r50 and r51 (~/.cache/tmp/carl-gfrest/c/pending/) once habu-report-a-refused-de5d1404 has landed.
Files: src/compiler/native/compiler.f, src/core/checker.f if the lookup lives there, test/outer-interpret.f, test/compiler/native-hookless-reject.f, test/gforth/cases/.
Verify: native build per docs/gate.md; `bin/hb --load test/run.f`; `bin/hb --load test/outer-interpret.f`.
Depends: none. Worker: worker-max.

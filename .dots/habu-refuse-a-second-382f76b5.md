---
title: Refuse a second while at tier 1
status: open
priority: 2
issue-type: task
created-at: "2026-10-10T03:30:36.664855+03:00"
---

Problem: ~/.cache/tmp/carl-gfrest/c/c12-while2-trusted.f (`trusted: F ( n -- n ) begin dup 0 > while dup 5 < while 1- repeat 100 + ;`) is refused at tier 0 at the second while (`hb: control-flow word does not match the open structure: while`, rc 70); tier 1 accepts it and prints 100 107 100, rc 0, under the Habu loop and the engine loop alike (master 9751f482). Native: src/compiler/native/elaborate.f:2559-2563 SK-WHILE counts every while on one begin (CS-WHILE+) and SK-REPEAT :2565-2571 closes them all at one repeat. By body kind: checked (c12-while2-checked.f) both tiers refuse, tier 1 through the checker (E-REJECTED at the second while), rc 70; TRUSTED: in that form, tier 0 refuses and tier 1 accepts; TRUSTED: in the ANS form `begin … while … while … repeat … then` (c12-while2-then.f), tier 0 refuses at the second while, rc 70, and tier 1 refuses -8502 E-NELAB-CTRL, rc 67, at the `then` that repeat left with no opener. The language has one while per begin: tools/check.f refuses the second while in both checked forms (E-REJECTED), docs/forth.md:1582-1590 (one `begin <cond> while <body> repeat`; malformed control syntax is a rejection), docs/effects.md:653. test/compiler/native-elaborate.f:998-1029 TWOW-CASE asserts the two-while acceptance, WWIDTH (:1155-1161, :1332-1334) expects E-NELAB-JOIN from a two-while body, and lib/errors.f:908 speaks of a loop's `while`s; no tree source outside those fixtures writes two whiles on one begin.
Acceptance: at tier 1 a second while on one begin is refused E-NELAB-CTRL at that while in every body kind: c12-while2-trusted.f prints nothing and exits 67 with -8502; c12-while2-then.f stays refused; checked bodies keep the checker's refusal. TWOW-CASE becomes a refusal beside TWOE-CASE (:1340-1342), WWIDTH's expectation follows, lib/errors.f:908 names one while. The reproducers join test/compiler/native-elaborate.f's control refusals, through the load path at tier 1 as test/compiler/native-again.f's while cases do (EV-DEF).
Files: src/compiler/native/elaborate.f, lib/errors.f, test/compiler/native-elaborate.f, test/compiler/native-again.f.
Verify: rebuild bin/hb per docs/gate.md; each reproducer under `bin/hb --load test/outer-loop-on.f <file holding 1 set-tier> <case>`; `bin/hb --load test/compiler/native-elaborate.f` and `bin/hb --load test/compiler/native-again.f`; `bin/hb --load test/run.f`.
Depends: none.
Worker: worker.

---
title: Undefined word in evaluate under catch crashes natively
status: open
priority: 2
issue-type: task
created-at: "2026-07-07T15:37:31.793971+02:00"
---

Engine bug found while building tools/codegen-role.f. Reproducer (bin/hb --load): TRUSTED: EV ( ptr u8 n -- ) evaluate ;  : GO ( -- ) s" : FOO ( -- ) UNDEFINED-WORD-XYZ ;" EV ;  : T1 ( -- ) [: GO ;] catch . cr ;  T1 -> prints E-UNDEFINED: UNDEFINED-WORD-XYZ then habu-crash register dump, exit 134 (SIGABRT path), instead of delivering a catchable throw code to the catch. Interpret-level undefined inside evaluate (no catch) prints and continues; the crash needs the undefined word to abort a mid-':'-definition compile inside evaluate under an active quotation catch frame. Owner: evaluate throw-recovery (LEVALREC/EM-EVAL-UNDEF-ROLLBACK, src/habu/habu2.f). Fix: undefined-word abort inside evaluate must unwind as a normal catchable throw across the catch frame. Regression: the reproducer as a checked fixture asserting a nonzero catch code and exit 0. tools/codegen-role.f avoids the path by CHECK!-certifying definitions before evaluate (E-CGR-EVAL verdict path).

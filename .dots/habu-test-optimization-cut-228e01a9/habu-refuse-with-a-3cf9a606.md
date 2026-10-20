---
title: Refuse ; with a control structure open
status: closed
priority: 3
issue-type: task
created-at: "2026-10-02T05:11:06.481475+02:00"
closed-at: "2026-10-14T19:30:56.927355+03:00"
close-reason: "Implemented in 347cd149: tier-0 semicolon refuses open control structures before publication, including caught-evaluate cases; red/green runtime regression and convergence recorded."
---

Problem: tier 0's ; (EM-COMPILE-SEMI, src/habu/habu2.f) publishes a TRUSTED: or checker-off definition whose IF has no THEN, leaving the forward branch's self-branch placeholder: 'TRUSTED: X ( -- ) 0 if ;' then 'X' hangs (timeout 15, rc 124, engine rb3/g1 on 53e02ad8); '1 if' and 'begin' return by luck. Same class as the open-quotation ; (dot f0a4c0e6, lane r4-semiquot d82c349f, which added the QPATCH-CELL refusal at the start of EM-COMPILE-SEMI and found this; probes $HOME/.cache/tmp/kestrel-r4-semiquot/p1.f p3.f). Acceptance: ; with any control-flow item open (IF/ELSE/AHEAD/BEGIN/WHILE/CASE/OF/DO, whatever tier 0's control stack holds) is refused by name before anything is published, like the open-quotation refusal; caught inside evaluate the session stays usable; rows in test/runtime-regression-test.f beside RXSC, seen failing first. Base: after d82c349f lands. Baked: rebuild, g1 == g2 with .names, two-generation build.

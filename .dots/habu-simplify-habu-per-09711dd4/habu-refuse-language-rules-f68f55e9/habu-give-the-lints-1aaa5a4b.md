---
title: Give the lints one definer list and tables sized from the scan
status: closed
priority: 2
issue-type: task
created-at: "2026-11-01T06:52:36.573814+02:00"
closed-at: "2026-11-10T22:41:52.029768+02:00"
close-reason: "split into habu-size-the-clobber-0e9d32d8 (the clobber lint's fixed tables, WORD-NAME! overrun and colon-only rows); the shadow-lint half goes with tools/lint/shadow-lint.f, which habu-refuse-language-rules-f68f55e9 deletes"
---

Problem: the clobber lint (tools/lint/clobber-lint-core.f) gives a row only to colon definitions, so a name another definer makes does not hide a global host word: /tmp/jerry-sealed-rv2-tmp/fx/pkgconst.f (`12 constant SCRATCH` in a package) counts 1, a false finding; no engine name has that shape today (sealed lint re-review, census). shadow-lint.f DEF-NAME-OFFSET keeps its own definer list (:, TRUSTED:, constant, variable, create, LAYOUT-BUFFER), without defer, value or the typed storage definers, so both lints miss the same names. The clobber lint also keeps fixed tables that predate lane sealed: CMAX 2048 (the label table holds 478 entries on the engine; every entry is countable by DECL-FILE, so it can be sized as HOSTS is), EMAX, CNBUF-CAP, OMAX, RRMAX and WNAME 128, which WORD-NAME! (~915) fills without a length check. Acceptance: one definer classification both lints read, every name a top-level definer makes gets a non-emitting row (pkgconst.f counts 0), the tables are sized from the scan and WORD-NAME! cannot overrun. Files: tools/lint/clobber-lint-core.f, tools/lint/shadow-lint.f and their tests. Depends: lane sealed landing (habu-answer-tools-check-745b0ccd).

Census 2026-11-04: a disagreement between two implementations of one thing; it is closed by its parent removing the second implementation, not by patching that copy.

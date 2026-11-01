---
title: Give the lints one definer list and tables sized from the scan
status: open
priority: 2
issue-type: task
created-at: "2026-11-01T06:52:36.573814+02:00"
---

Problem: the clobber lint (tools/lint/clobber-lint-core.f) gives a row only to colon definitions, so a name another definer makes does not hide a global host word: /tmp/jerry-sealed-rv2-tmp/fx/pkgconst.f (`12 constant SCRATCH` in a package) counts 1, a false finding; no engine name has that shape today (sealed lint re-review, census). shadow-lint.f DEF-NAME-OFFSET keeps its own definer list (:, TRUSTED:, constant, variable, create, LAYOUT-BUFFER), without defer, value or the typed storage definers, so both lints miss the same names. The clobber lint also keeps fixed tables that predate lane sealed: CMAX 2048 (the label table holds 478 entries on the engine; every entry is countable by DECL-FILE, so it can be sized as HOSTS is), EMAX, CNBUF-CAP, OMAX, RRMAX and WNAME 128, which WORD-NAME! (~915) fills without a length check. Acceptance: one definer classification both lints read, every name a top-level definer makes gets a non-emitting row (pkgconst.f counts 0), the tables are sized from the scan and WORD-NAME! cannot overrun. Files: tools/lint/clobber-lint-core.f, tools/lint/shadow-lint.f and their tests. Depends: lane sealed landing (habu-answer-tools-check-745b0ccd).

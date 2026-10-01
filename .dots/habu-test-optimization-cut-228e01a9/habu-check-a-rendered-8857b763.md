---
title: Check a rendered product named like an engine global as the load does
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T11:48:58.761413+02:00"
---

Problem: tools/check.f judges a rendered product (FUNCTION:, COMMAND, +USER, TASK:+USER) whose name an engine global already takes against that global, so check and load disagree: test/check rendered-shadow (r4-render 66fbaced) records BYTE-COPY as load 0, check 70 E-INPUT-UNDERFLOW; review 58 cases r-shadow-baked and x-shadow-lazy in $HOME/.cache/tmp/kestrel-r4-rev58/cases/. The deferral in f02aa703 covers only names nothing visible defines yet; bin/hb carries no seeded pool, so a covered global never reaches the lazy intake. Acceptance: check.f gives the load's verdict for those three cases (a product shadowing a global is deferred to the run, or judged against its rendered text), a wrong effect in such a product is still refused, the check/rendered-shadow case flips to the load's verdict and is seen failing first; docs/forth.md drops the stated limit. Files: src/core/checker.f, src/habu/verify-source.f, tools/check-test-lib.f, docs/forth.md.

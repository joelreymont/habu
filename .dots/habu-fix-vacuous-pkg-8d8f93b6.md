---
title: Fix vacuous package-name privacy probes
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T11:40:47.449327+02:00"
---

Test-integrity defect found by the wave B1 lane, pre-existing: search-wl ( ptr u8 wid -- xt ) searches one wordlist for a literal name, so a package-qualified string like CONFIG:RAW>CONFIG-ID can never be found there - the privacy assertions of the form s-quote PKG:WORD 0 search-wl 0= TTRUE pass for PUBLIC words too (measured: CONFIG:REGISTER, a public word, is also absent). Every suite in the id-result family carries this shape (maki/config-test.f, producer-test.f, rev-test.f, schema-test.f, journal-test.f at least; sweep for the pattern repo-wide). Behavior: design one real cross-package privacy probe and replace the vacuous assertions - the probe must distinguish private from misspelled, which the bare checker verdict cannot (verdict 1 covers both), so it needs a paired control: the private word yields unresolvable AND a public sibling from the same package yields certified in the same probe, making a typo in the package prefix fail the control. Put the probe helper in the shared test-support package so fifty suites do not reinvent it. Hostile fixtures: a public word asserted private must fail; a misspelled package prefix must fail the control, not pass as private. Acceptance: the replaced assertions in the swept suites; the probe helper own focused test; maki/test.f green. Owner: the shared checker-assert test-support package. Dependencies: none.

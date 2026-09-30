---
title: Delete dead libraries and give kept ones real tests
status: closed
priority: 2
issue-type: task
created-at: "2026-09-30T15:12:49.891921+02:00"
closed-at: "2026-09-30T15:13:06.408486+02:00"
close-reason: "Landed a24e88d7..bc2301df (10 commits: stats, table, report/render, regex, boxed-layout arena, property generator and Unicode provenance words deleted; PAGE-SIZE vs getconf; pg on a private cluster; f64-text and map key docs). Fable review accepted. Native gate 504/504 rc 0, 281.2 s wall, 1984 s pooled at load 78-105 (gated at d4d16008, pushed as d8867baf after rebasing over a dots-only commit)."
---

Problem: the test review found libraries whose tests were their only consumer (stats, table, report and render formatters, regex, the boxed-layout arena, property generator and Unicode provenance words), a PAGE-SIZE test that restated the constant, and pg tests that never reached a server. Acceptance: libraries with no production consumer are deleted with their tests; PAGE-SIZE is checked against getconf; pg runs against a private PostgreSQL cluster the harness starts and stops; f64-text and map key lifetimes are documented. Verify: each changed test file passes standalone; rg finds no reference to a deleted word; full native gate.

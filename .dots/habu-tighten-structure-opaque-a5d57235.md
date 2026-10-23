---
title: Tighten structure-opaque FORGE check
status: open
priority: 3
issue-type: task
created-at: "2026-10-23T17:34:38.072996+03:00"
---

Problem: in test/structure-opaque-e2e.f, FORGE only checks that stderr contains the forge packet, so a broken OP-internal call in the fixture (WRAP ... BOX-MAKE BOX-MAKE) still passes FORGE while check.f reports the WRAP mismatch; only VERIFY catches it. The file's comment says check 'stops at the first refusal', but check reported both refusals. Separately, require-cap's second case is labelled 'grows again' although 650 causes no further growth (the table doubles from 64; 513 and 650 share the 1024 step). (g6 test audit lane.)
Acceptance: FORGE asserts its stderr holds only the forge packet, so the WRAP mutation fails it; the opaque comment and the require-cap label state what the cases prove.
Verification: both suites pass; the WRAP mutation turns structure-opaque's FORGE red.

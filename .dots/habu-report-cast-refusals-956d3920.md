---
title: Report cast refusals as coded diagnostics
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T19:37:36.750895+02:00"
---

Problem: every CAST: refusal (E-CAST-MINT 7147, E-CAST-FAM 7131, E-CAST-OWNER 7135) reaches tools/check.f, default and --verify-only, as E-STATEMENT-THROW with a throw_code, and stops verification, because CHECKER-DEFCAST (src/core/checker.f, about :26047) and CAST-CERTIFY refuse with a bare throw. Storage refusals emit a coded diagnostic through CHECKER-STORAGE-TYPE-REFUSE. Found by the ARENA round-2 review on the lane engine and the installed hb. Requirement: a refused cast is reported under its own code with file:line, as storage refusals are. Acceptance: --verify-only on a file holding a refused cast names E-CAST-* and its line; test/arena.f VERIFY-CALLS (pins 'verification stopped by throw 7135') updated to the coded form. Files: src/core/checker.f, tools/verify-source.f, test/arena.f. Verify: test/arena.f, tools/check.f --verify-only rows. Depends: ARENA landing (habu-land-the-arena-f9d29370).

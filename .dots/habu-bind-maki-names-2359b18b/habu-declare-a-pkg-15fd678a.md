---
title: Declare a package complete in source
status: active
priority: 2
issue-type: task
created-at: "2026-11-01T15:36:36.175193+02:00"
---

Maki H2: a source declaration that a package is complete, honoured by the checker's scan and by the run, so after an import file closes its package a later package NAME in a design file is refused by name at check instead of merging. The import's own file reopens its package several times before it closes it. Needs a design together with H1; public names to Joel via tim. Claim: carl.

Design (Joel 2026-11-01, LESSONS "sealing a package": "One word, seal-package"): `seal-package ( -- )`, written inside the package; the run protects its public and private wordlists (rc 75 outside a package); the checker's scan reads it by name and keeps a per-compose list of sealed packages, so a later reopen, qualified or unqualified definition, or export into one is E-SEALED-PACKAGE at check and at `--verify-only` (today the idiom passes `--verify-only`, /tmp/carl-seal); it replaces the `private get-current prot-wid-add public get-current prot-wid-add` idiom in the engine's 64 files; no automatic seal at app image save. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-seal brief=carl-dots/.out/carl-file/brief-seal-package.md.

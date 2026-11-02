---
title: Declare a package complete in source
status: closed
priority: 2
issue-type: task
created-at: "2026-11-01T15:36:36.175193+02:00"
closed-at: "2026-11-02T19:02:57.549855+02:00"
close-reason: "nuylyxrr 331dc067 seal-package; reviewed ACCEPT; seal fixtures and rows unchanged on the batch hb; no-binary check rc 0 on the lane; landed in batch 1 on master 1fc572b1: full suite 515/515 rc 0 and two-generation gen 3 = gen 2 on the batch build (/tmp/carl-land/b1)"
---

Maki H2: a source declaration that a package is complete, honoured by the checker's scan and by the run, so after an import file closes its package a later package NAME in a design file is refused by name at check instead of merging. The import's own file reopens its package several times before it closes it. Needs a design together with H1; public names to Joel via tim. Claim: carl.

Design (Joel 2026-11-01, LESSONS "sealing a package": "One word, seal-package"): `seal-package ( -- )`, written inside the package; the run protects its public and private wordlists (rc 75 outside a package); the checker's scan reads it by name and keeps a per-compose list of sealed packages, so a later reopen, qualified or unqualified definition, or export into one is E-SEALED-PACKAGE at check and at `--verify-only` (today the idiom passes `--verify-only`, /tmp/carl-seal); it replaces the `private get-current prot-wid-add public get-current prot-wid-add` idiom in the engine's 64 files; no automatic seal at app image save. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-seal brief=carl-dots/.out/carl-file/brief-seal-package.md.


Status 2026-11-02: fold and re-review accepted; rebased onto master 2e6258d4 with the lead's comment fold-ins. Landing-proof worker running in carl-seal (brief .out/carl-file/brief-seal-land.md: stage host from the 9c1e8687 checker patch, focused rows, full suite, two-generation build, no-binary check); then land and install.

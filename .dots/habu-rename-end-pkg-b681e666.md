---
title: Rename end-package to ;package (engine keyword, two-phase)
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T18:18:24.256335+02:00"
---

User rule: block pairs are FOO … ;FOO; package keyword pair becomes package … ;package (keyword case follows opener). Engine-level rename, ~119 files use end-package. Two-phase self-host dance: (1) add ;package as accepted keyword alongside end-package in native keyword table src/habu/habu2.f:3101 (LKWENDPACKAGE/C-END-PACKAGE CF-ENTRY; add LKWSEMIPKG row) + src/habu/verify-source.f:412 RECORD-END-PACKAGE match + checker package recorder (checker-end-package label) + tools that parse the token (public-signatures-core.f, check-all-errors-core.f, trusted-inventory.f, gate-dictionary-lib fixtures); rebuild bin/hb via fixpoint; (2) sweep all sources/docs end-package -> ;package (PLAN.md:429, docs/forth.md § Packages, lib/, tools/, test/, maki/); (3) drop end-package from the keyword tables, rebuild again, add negative regression proving end-package now fails E-UNDEFINED on the checked path. Full native gate + bootstrap seed compatibility check (docs/seed.md path must still work) after each phase. SEQUENCE: after in-flight workers merge; conflicts with everything (whole-tree sweep) so run solo, no parallel workers.

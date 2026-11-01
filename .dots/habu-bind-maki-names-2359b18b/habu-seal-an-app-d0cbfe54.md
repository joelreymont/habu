---
title: "Seal an application's packages before its image is saved"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-01T15:36:36.162524+02:00"
closed-at: "2026-11-01T16:23:26.113825+02:00"
close-reason: "Answered by seal-package (habu-declare-a-pkg-15fd678a): an application writes seal-package in a package's last file before saving; Joel 2026-11-01 chose no automatic seal at image save (LESSONS 'sealing a package')"
---

Maki H1: a public, documented way for an application to seal its packages before saving its image, so a later package NAME in design or import source naming one is refused by name at check (E-SEALED-PACKAGE) instead of reopening it. The engine already seals its baked packages (packages.f PKG-SEALED? ~149-157, verify-source.f ~2154, docs/forth.md ~187: 'An application image keeps its own packages reopenable'). Protects package names only, not VOCABULARY:RESTRICT. Needs a design; public names to Joel via tim. Claim: carl.

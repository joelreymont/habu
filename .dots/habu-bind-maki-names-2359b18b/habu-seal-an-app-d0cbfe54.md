---
title: "Seal an application's packages before its image is saved"
status: open
priority: 2
issue-type: task
created-at: "2026-11-01T15:36:36.162524+02:00"
---

Maki H1: a public, documented way for an application to seal its packages before saving its image, so a later package NAME in design or import source naming one is refused by name at check (E-SEALED-PACKAGE) instead of reopening it. The engine already seals its baked packages (packages.f PKG-SEALED? ~149-157, verify-source.f ~2154, docs/forth.md ~187: 'An application image keeps its own packages reopenable'). Protects package names only, not VOCABULARY:RESTRICT. Needs a design; public names to Joel via tim. Claim: carl.

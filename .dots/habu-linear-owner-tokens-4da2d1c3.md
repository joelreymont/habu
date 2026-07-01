---
title: Linear owner tokens for arena records
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T19:05:53.735755+02:00"
---

Value records now cover by-value stack records: nominal effect tokens, construction/destructure, copy/access/update, polymorphic fields, duplicate-field rejection, docs, and native suite proof. Remaining ownership/lifetime acceptance from habu-first-class-value-2c794629 is a separate checker capability: add affine/noncopyable owner tokens so arena-backed records can carry lifetime/owner evidence that DUP/OVER/2DUP/stores cannot duplicate or leak. Files: src/core/checker.f primitive effects/copyability model, docs/effects.md, docs/forth.md, focused negative fixtures for owner dup/leak/lifetime mismatch, then migrate an arena-backed structure path.

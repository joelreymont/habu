---
title: Load typed rows into PostgreSQL in bulk
status: active
priority: 2
issue-type: task
created-at: "2026-11-23T19:19:15.465632+01:00"
---

Problem: Maki's no-text form (tim, Joel approved 2026-11-23) stores a design step's operations as typed rows; today DB:OPS+ (maki src/db.f:290-300) sends one jsonb array, 100,063 rows in 1.0-1.4 s, and src/lib/pg.f:988-991 rejects COPY. Acceptance: a Habu program loads many typed rows (uuid, bytea, int, bool, text, bigint, null) in one round trip with COPY FROM STDIN binary, and learns whether it worked. Depends: none. Claim: agent=carl-pgcopy-plan (design) workspace=none.

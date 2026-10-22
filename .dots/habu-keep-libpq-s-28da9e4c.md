---
title: "Keep libpq's early connect errors without secrets"
status: open
priority: 2
issue-type: task
created-at: "2026-10-22T13:19:23.375297+03:00"
---

Problem (reported by kevin): PG:CONNECT throws E-CONNECT (-9250) with no libpq text for DNS failure, a missing socket directory and malformed conninfo. src/lib/pg.f:1116-1118 CONNECT-START: PQsetnonblocking returns -1 on CONNECTION_BAD before the first poll, so FAIL-CONNECTION throws E-CONNECT and these never reach the 'refused' path; only failures after the first poll (TCP refused, auth, server FATAL) carry text. Reproducer: hb --load /Users/joel/.cache/tmp/kevin/habu-pg/pgmsg.f (C1-C5 print -9250 with no text; C6 prints the refusal; trailing E-UNDEFINED: bye is harmless). Security constraint: libpq's conninfo-parser messages echo conninfo fragments including the password (URI and percent-escape errors, C4/C5), and tenderd logs refusal text, so the text must not be passed through unchanged. Acceptance: check CONNECTION_BAD before PQsetnonblocking and surface PQerrorMessage for C1-C3; for conninfo-parse failures report a message that cannot contain the password (design choice: redact, or a fixed message naming the failing part). Files: src/lib/pg.f. Verify: pgmsg.f C1-C6 plus the existing pg tests. Depends: none. Ownership: PG library owner. Claim: unassigned.

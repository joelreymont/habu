---
title: Read bytea results as bytes through PG
status: open
priority: 2
issue-type: task
created-at: "2026-10-23T10:48:30.962467+03:00"
---

Problem: PG returns every result column as text: each query passes the result format TEXT-RESULT (src/lib/pg.f:94, used at :1119, :1134 and :1214), and PostgreSQL applies one result format to the whole statement, so a bytea column reaches the caller as text. Maki's project store therefore base64-encodes every scene artifact through PostgreSQL encode/decode on every import and every /scene read. Binary parameters landed (PG:BINARY+, src/lib/pg.f:1517, d8ef9e05; DECISIONS.md, the binary statement parameter entry), so only the read side is left. Reading binary results needs a new public word, not yet chosen.

Acceptance: the public word is decided and recorded in DECISIONS.md: how a caller asks for binary results and how it reads a column's bytes and length. Then a round-trip of arbitrary bytes, including NUL, through a bytea column reads back byte-equal, and text-only statements are unchanged.

Files: src/lib/pg.f, src/lib/pg-test.f, DECISIONS.md.

Verify: a round-trip case in src/lib/pg-test.f.

Depends: none. Caller: Maki src/project-store.f scene artifacts.

Ownership: unassigned. Claim: unassigned.

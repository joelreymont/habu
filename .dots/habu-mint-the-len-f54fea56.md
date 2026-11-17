---
title: Mint the length and count roles only from checked cells
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T17:04:25.006761+01:00"
---

Problem: the global roles len, idx, count and off are minted by unchecked global casts (src/core/roles.f >LEN, >IDX, >COUNT, >OFF; string.f STR-LEN, STR-OFF, STR-COUNT wrap them), used about a thousand times, so a negative cell becomes a len: `s" x" drop -3 buf 16 cell BUF-APPEND` stores -3 into the buffer's length cell, and `SB-RESET s" abc" SB-APPEND s" x" drop -2 SB-APPEND SB$ nip .` moves the builder back to 1 (src/lib/string.f BUF-APPEND-LEN; BUF-CHECK-LEN tests only the upper bound), and STR:BUF-LEN@ must validate the cell again. NUM's roles (src/lib/num-types.f) are minted only by a validator or by a cast in a package that has shown the value is the role. Acceptance: no global cast mints a count, length, offset or index role from an unchecked cell; the BUF-APPEND program above is rejected by the checker or throws E-STR-BOUNDS before it stores; one role per meaning across string.f and NUM. Strings are `str` (dot 9f752b95), so a string's length is checked where a str is made, and string words take no separate length. A role's least value is a NEWTYPE clause, `NEWTYPE len LEAST 0` (the user chose LEAST, 2026-11-17). Depends: 9f752b95; a design re-cut onto str. Claim: unassigned.
